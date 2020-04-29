/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstSemanticChecker.cpp
 *
 * Implementation of the semantic checker pass.
 *
 ***********************************************************************/

#include "AstSemanticChecker.h"
#include "AstArgument.h"
#include "AstAttribute.h"
#include "AstClause.h"
#include "AstFunctorDeclaration.h"
#include "AstGroundAnalysis.h"
#include "AstIO.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstQualifiedName.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "AstTypeAnalysis.h"
#include "AstTypeEnvironmentAnalysis.h"
#include "AstUtils.h"
#include "AstVisitor.h"
#include "BinaryConstraintOps.h"
#include "ErrorReport.h"
#include "FunctorOps.h"
#include "Global.h"
#include "GraphUtils.h"
#include "PrecedenceGraph.h"
#include "RamTypes.h"
#include "RelationTag.h"
#include "SrcLocation.h"
#include "TypeSystem.h"
#include "Util.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <typeinfo>
#include <utility>
#include <vector>

namespace souffle {

struct AstSemanticCheckerImpl {
    AstTranslationUnit& tu;
    AstSemanticCheckerImpl(AstTranslationUnit& tu);

private:
    const IOType& ioTypes = *tu.getAnalysis<IOType>();
    const PrecedenceGraph& precedenceGraph = *tu.getAnalysis<PrecedenceGraph>();
    const RecursiveClauses& recursiveClauses = *tu.getAnalysis<RecursiveClauses>();
    const TypeAnalysis& typeAnalysis = *tu.getAnalysis<TypeAnalysis>();
    const TypeEnvironmentAnalysis& typeEnvAnalysis = *tu.getAnalysis<TypeEnvironmentAnalysis>();
    const SCCGraph& sccGraph = *tu.getAnalysis<SCCGraph>();

    const TypeEnvironment& typeEnv = typeEnvAnalysis.getTypeEnvironment();
    const AstProgram& program = *tu.getProgram();
    ErrorReport& report = tu.getErrorReport();

    void checkAtom(const AstAtom& atom);
    void checkLiteral(const AstLiteral& literal);
    void checkAggregator(const AstAggregator& aggregator);
    bool isDependent(const AstClause& agg1, const AstClause& agg2);
    void checkArgument(const AstArgument& arg);
    void checkConstant(const AstArgument& argument);
    void checkFact(const AstClause& fact);
    void checkClause(const AstClause& clause);
    void checkRelationDeclaration(const AstRelation& relation);
    void checkRelation(const AstRelation& relation);

    void checkTypes();
    void checkRecursiveUnionTypes();
    void checkType(const AstType& type);
    void checkRecordType(const AstRecordType& type);
    void checkUnionType(const AstUnionType& type);

    void checkNamespaces();
    void checkIO();
    void checkWitnessProblem();
    void checkInlining();
};

bool AstSemanticChecker::transform(AstTranslationUnit& translationUnit) {
    AstSemanticCheckerImpl{translationUnit};
    return false;
}

AstSemanticCheckerImpl::AstSemanticCheckerImpl(AstTranslationUnit& tu) : tu(tu) {
    // suppress warnings for given relations
    if (Global::config().has("suppress-warnings")) {
        std::vector<std::string> suppressedRelations =
                splitString(Global::config().get("suppress-warnings"), ',');

        if (std::find(suppressedRelations.begin(), suppressedRelations.end(), "*") !=
                suppressedRelations.end()) {
            // mute all relations
            for (AstRelation* rel : program.getRelations()) {
                rel->addQualifier(RelationQualifier::SUPPRESSED);
            }
        } else {
            // mute only the given relations (if they exist)
            for (auto& relname : suppressedRelations) {
                const std::vector<std::string> comps = splitString(relname, '.');
                if (!comps.empty()) {
                    // generate the relation identifier
                    AstQualifiedName relid(comps[0]);
                    for (size_t i = 1; i < comps.size(); i++) {
                        relid.append(comps[i]);
                    }

                    // update suppressed qualifier if the relation is found
                    if (AstRelation* rel = getRelation(program, relid)) {
                        rel->addQualifier(RelationQualifier::SUPPRESSED);
                    }
                }
            }
        }
    }

    // -- conduct checks --
    // TODO: re-write to use visitors
    checkTypes();

    // check rules
    for (auto* rel : program.getRelations()) checkRelation(*rel);
    for (auto* clause : program.getClauses()) checkClause(*clause);

    checkNamespaces();
    checkIO();
    checkWitnessProblem();
    checkInlining();

    // Run grounded terms checker
    GroundedTermsChecker().verify(tu);

    // get the list of components to be checked (clauses w/ relation decls)
    std::vector<const AstNode*> nodes;
    for (const auto& rel : program.getRelations()) {
        for (const auto& cls : getClauses(program, *rel)) {
            nodes.push_back(cls);
        }
    }

    // -- type checks --

    // check if in all atoms the arguments are subtypes of declared types.
    visitDepthFirst(nodes, [&](const AstAtom& atom) {
        auto relation = getAtomRelation(&atom, &program);
        if (relation == nullptr) {
            return;  // error unrelated to types.
        }

        auto attributes = relation->getAttributes();
        auto arguments = atom.getArguments();
        if (attributes.size() != arguments.size()) {
            return;  // error in input program
        }

        for (size_t i = 0; i < attributes.size(); ++i) {
            auto& typeName = attributes[i]->getTypeName();
            if (typeEnv.isType(typeName)) {
                auto argTypes = typeAnalysis.getTypes(arguments[i]);
                auto& attributeType = typeEnv.getType(typeName);

                if (argTypes.isAll() || argTypes.empty()) {
                    continue;  // This will be reported later.
                }

                // Attribute and argument type agree if, argument type is a subtype of declared type or is of
                // the appropriate constant type.
                bool validAttribute = all_of(argTypes, [&attributeType](const Type& type) {
                    return isSubtypeOf(type, attributeType) ||
                           (isA<ConstantType>(type) && isSubtypeOf(attributeType, type));
                });
                if (!validAttribute) {
                    if (Global::config().has("legacy")) {
                        report.addWarning("Atoms argument type is not a subtype of its declared type",
                                arguments[i]->getSrcLoc());
                    } else {
                        report.addError("Atoms argument type is not a subtype of its declared type",
                                arguments[i]->getSrcLoc());
                    }
                }
            }
        }
    });

    // - variables -
    visitDepthFirst(nodes, [&](const AstVariable& var) {
        if (typeAnalysis.getTypes(&var).empty()) {
            report.addError("Unable to deduce type for variable " + var.getName(), var.getSrcLoc());
        }
    });

    // - constants -

    // all string constants are used as symbols
    visitDepthFirst(nodes, [&](const AstStringConstant& constant) {
        TypeSet types = typeAnalysis.getTypes(&constant);
        if (!isSymbolType(types)) {
            report.addError("Symbol constant (type mismatch)", constant.getSrcLoc());
        }
    });

    // TODO (darth_tytus): Improve error messages.
    // At this stage each constant should have a type (assigned by a transformer)
    visitDepthFirst(nodes, [&](const AstNumericConstant& constant) {
        TypeSet types = typeAnalysis.getTypes(&constant);

        // No type could be assigned.
        if (!constant.getType().has_value()) {
            report.addError("Ambiguous constant (unable to deduce type)", constant.getSrcLoc());
            return;
        }

        switch (*constant.getType()) {
            case AstNumericConstant::Type::Int:
                if (!hasSignedType(types)) {
                    report.addError("Number constant (type mismatch)", constant.getSrcLoc());
                }
                break;
            case AstNumericConstant::Type::Uint:
                if (!hasUnsignedType(types)) {
                    report.addError("Unsigned constant (type mismatch)", constant.getSrcLoc());
                }
                break;
            case AstNumericConstant::Type::Float:
                if (!hasFloatType(types)) {
                    report.addError("Float constant (type mismatch)", constant.getSrcLoc());
                }
                break;
        }
    });

    // all nil constants are used as records
    visitDepthFirst(nodes, [&](const AstNilConstant& constant) {
        TypeSet types = typeAnalysis.getTypes(&constant);
        if (!isRecordType(types)) {
            report.addError("Nil constant used as a non-record", constant.getSrcLoc());
        }
    });

    // record initializations have the same size as their types
    visitDepthFirst(nodes, [&](const AstRecordInit& constant) {
        TypeSet types = typeAnalysis.getTypes(&constant);

        if (!isRecordType(types) || types.size() != 1) {
            report.addError("Ambiguous record", constant.getSrcLoc());
            return;
        }

        // At this point we know that there is exactly one type in set, so we can take it.
        const RecordType* recordType = dynamic_cast<const RecordType*>(&(*types.begin()));

        if (recordType->getFields().size() != constant.getArguments().size()) {
            report.addError("Wrong number of arguments given to record", constant.getSrcLoc());
        }
    });

    // type casts name a valid type
    visitDepthFirst(nodes, [&](const AstTypeCast& cast) {
        if (!typeEnv.isType(cast.getType())) {
            report.addError("Type cast is to undeclared type " + toString(cast.getType()), cast.getSrcLoc());
        }
    });

    // - functors -
    visitDepthFirst(nodes, [&](const AstFunctor& fun) {
        // check type of result
        const TypeSet& resultType = typeAnalysis.getTypes(&fun);

        if (!eqTypeTypeAttribute(fun.getReturnType(), resultType)) {
            switch (fun.getReturnType()) {
                case TypeAttribute::Signed:
                    report.addError("Non-numeric use for numeric functor", fun.getSrcLoc());
                    break;
                case TypeAttribute::Unsigned:
                    report.addError("Non-unsigned use for unsigned functor", fun.getSrcLoc());
                    break;
                case TypeAttribute::Float:
                    report.addError("Non-float use for float functor", fun.getSrcLoc());
                    break;
                case TypeAttribute::Symbol:
                    report.addError("Non-symbolic use for symbolic functor", fun.getSrcLoc());
                    break;
                case TypeAttribute::Record:
                    fatal("Invalid return type");
            }
        }

        // Special case
        if (auto intrFun = dynamic_cast<const AstIntrinsicFunctor*>(&fun)) {
            if (intrFun->getFunction() == FunctorOp::ORD) {
                return;
            }
        }

        size_t i = 0;
        for (auto arg : fun.getArguments()) {
            if (!eqTypeTypeAttribute(fun.getArgType(i), typeAnalysis.getTypes(arg))) {
                switch (fun.getArgType(i)) {
                    case TypeAttribute::Signed:
                        report.addError("Non-numeric argument for functor", arg->getSrcLoc());
                        break;
                    case TypeAttribute::Symbol:
                        report.addError("Non-symbolic argument for functor", arg->getSrcLoc());
                        break;
                    case TypeAttribute::Unsigned:
                        report.addError("Non-unsigned argument for functor", arg->getSrcLoc());
                        break;
                    case TypeAttribute::Float:
                        report.addError("Non-float argument for functor", arg->getSrcLoc());
                        break;
                    case TypeAttribute::Record:
                        fatal("Invalid argument type");
                }
            }
            ++i;
        }
    });

    // - binary relation -
    visitDepthFirst(nodes, [&](const AstBinaryConstraint& constraint) {
        auto op = constraint.getOperator();
        auto left = constraint.getLHS();
        auto right = constraint.getRHS();
        auto opRamTypes = getBinaryConstraintTypes(op);
        // Skip checks if either side is `Bottom` b/c it just adds noise.
        // The unable-to-deduce-type checker will point out the issue.
        if (typeAnalysis.getTypes(left).empty() || typeAnalysis.getTypes(right).empty()) return;

        // give them a slightly nicer error
        if (isOrderedBinaryConstraintOp(op) && typeAnalysis.getTypes(left) != typeAnalysis.getTypes(right)) {
            report.addError("Cannot compare different types", constraint.getSrcLoc());
        } else {
            auto checkTyAttr = [&](AstArgument const& side) {
                auto opMatchesType = any_of(opRamTypes, [&](auto& ramType) {
                    return eqTypeTypeAttribute(ramType, typeAnalysis.getTypes(&side));
                });

                if (!opMatchesType) {
                    std::stringstream ss;
                    ss << "Constraint requires an operand of type "
                       << join(opRamTypes, " or ", [&](auto& out, auto& ramTy) {
                              switch (ramTy) {
                                  case TypeAttribute::Signed:
                                      out << "`number`";
                                      break;
                                  case TypeAttribute::Symbol:
                                      out << "`symbol`";
                                      break;
                                  case TypeAttribute::Unsigned:
                                      out << "`unsigned`";
                                      break;
                                  case TypeAttribute::Float:
                                      out << "`float`";
                                      break;
                                  case TypeAttribute::Record:
                                      out << "a record";
                                      break;
                              }
                          });
                    report.addError(ss.str(), side.getSrcLoc());
                }
            };

            checkTyAttr(*left);
            checkTyAttr(*right);
        }
    });

    visitDepthFirst(nodes, [&](const AstAggregator& aggregator) {
        auto op = aggregator.getOperator();

        auto aggregatorType = typeAnalysis.getTypes(&aggregator);

        TypeAttribute opType = getTypeAttributeAggregate(op);

        // Check if operation type and return type agree.
        if (!eqTypeTypeAttribute(opType, aggregatorType)) {
            report.addError("Couldn't assign types to the aggregator", aggregator.getSrcLoc());
        }
    });

    // - stratification --

    // check for cyclic dependencies
    for (AstRelation* cur : program.getRelations()) {
        size_t scc = sccGraph.getSCC(cur);
        if (sccGraph.isRecursive(scc)) {
            for (const AstRelation* cyclicRelation : sccGraph.getInternalRelations(scc)) {
                // Negations and aggregations need to be stratified
                const AstLiteral* foundLiteral = nullptr;
                bool hasNegation = hasClauseWithNegatedRelation(cyclicRelation, cur, &program, foundLiteral);
                if (hasNegation ||
                        hasClauseWithAggregatedRelation(cyclicRelation, cur, &program, foundLiteral)) {
                    auto const& relSet = sccGraph.getInternalRelations(scc);
                    std::set<const AstRelation*, AstNameComparison> sortedRelSet(
                            relSet.begin(), relSet.end());
                    // Negations and aggregations need to be stratified
                    std::string relationsListStr = toString(join(sortedRelSet, ",",
                            [](std::ostream& out, const AstRelation* r) { out << r->getQualifiedName(); }));
                    std::vector<DiagnosticMessage> messages;
                    messages.push_back(DiagnosticMessage(
                            "Relation " + toString(cur->getQualifiedName()), cur->getSrcLoc()));
                    std::string negOrAgg = hasNegation ? "negation" : "aggregation";
                    messages.push_back(
                            DiagnosticMessage("has cyclic " + negOrAgg, foundLiteral->getSrcLoc()));
                    report.addDiagnostic(Diagnostic(Diagnostic::ERROR,
                            DiagnosticMessage("Unable to stratify relation(s) {" + relationsListStr + "}"),
                            messages));
                    break;
                }
            }
        }
    }
}

void AstSemanticCheckerImpl::checkAtom(const AstAtom& atom) {
    // check existence of relation
    auto* r = getRelation(program, atom.getQualifiedName());
    if (r == nullptr) {
        report.addError("Undefined relation " + toString(atom.getQualifiedName()), atom.getSrcLoc());
    }

    // check arity
    if ((r != nullptr) && r->getArity() != atom.getArity()) {
        report.addError(
                "Mismatching arity of relation " + toString(atom.getQualifiedName()), atom.getSrcLoc());
    }

    for (const AstArgument* arg : atom.getArguments()) {
        checkArgument(*arg);
    }
}

/* Check whether an unnamed variable occurs in an argument (expression) */
// TODO (azreika): use a visitor instead
static bool hasUnnamedVariable(const AstArgument* arg) {
    if (dynamic_cast<const AstUnnamedVariable*>(arg) != nullptr) {
        return true;
    }
    if (dynamic_cast<const AstVariable*>(arg) != nullptr) {
        return false;
    }
    if (dynamic_cast<const AstConstant*>(arg) != nullptr) {
        return false;
    }
    if (dynamic_cast<const AstCounter*>(arg) != nullptr) {
        return false;
    }
    if (const auto* cast = dynamic_cast<const AstTypeCast*>(arg)) {
        return hasUnnamedVariable(cast->getValue());
    }
    if (const auto* term = dynamic_cast<const AstTerm*>(arg)) {
        return any_of(term->getArguments(), hasUnnamedVariable);
    }
    if (dynamic_cast<const AstAggregator*>(arg) != nullptr) {
        return false;
    }

    fatal("unsupported argument type: %s", typeid(*arg).name());
}

static bool hasUnnamedVariable(const AstLiteral* lit) {
    if (const auto* at = dynamic_cast<const AstAtom*>(lit)) {
        return any_of(at->getArguments(), (bool (*)(const AstArgument*))hasUnnamedVariable);
    }
    if (const auto* neg = dynamic_cast<const AstNegation*>(lit)) {
        return hasUnnamedVariable(neg->getLiteral());
    }
    if (const auto* body = dynamic_cast<const AstBody*>(lit)) {
        return any_of(body->disjunction, [](auto&& conj) {
            return any_of(conj, [](auto&& lit) { return hasUnnamedVariable(lit.get()); });
        });
    }
    if (dynamic_cast<const AstConstraint*>(lit) != nullptr) {
        if (dynamic_cast<const AstBooleanConstraint*>(lit) != nullptr) {
            return false;
        }
        if (const auto* br = dynamic_cast<const AstBinaryConstraint*>(lit)) {
            return hasUnnamedVariable(br->getLHS()) || hasUnnamedVariable(br->getRHS());
        }
    }

    fatal("unsupported literal type: %s", typeid(*lit).name());
}

void AstSemanticCheckerImpl::checkLiteral(const AstLiteral& literal) {
    // check potential nested atom
    if (const auto* atom = dynamic_cast<const AstAtom*>(&literal)) {
        checkAtom(*atom);
    }

    if (const auto* constraint = dynamic_cast<const AstBinaryConstraint*>(&literal)) {
        checkArgument(*constraint->getLHS());
        checkArgument(*constraint->getRHS());
    }

    // check for invalid underscore utilization
    if (hasUnnamedVariable(&literal)) {
        if (isA<AstBody>(literal) || isA<AstAtom>(literal) || isA<AstNegation>(literal)) {
            // bodies, atoms, and negations all allow underscores
        } else if (isA<AstBinaryConstraint>(literal)) {
            report.addError("Underscore in binary relation", literal.getSrcLoc());
        } else {
            fatal("unsupported literal type: %s", typeid(literal).name());
        }
    }
}
/**
 * agg1, agg2 are clauses which contain no head, and consist of a single literal
 * that contains an aggregate.
 * agg1 is dependent on agg2 if agg1 contains a variable which is grounded by agg2, and not by agg1.
 */
bool AstSemanticCheckerImpl::isDependent(const AstClause& agg1, const AstClause& agg2) {
    auto groundedInAgg1 = getGroundedTerms(tu, agg1);
    auto groundedInAgg2 = getGroundedTerms(tu, agg2);
    bool dependent = false;
    // For each variable X in the first aggregate
    visitDepthFirst(agg1, [&](const AstVariable& searchVar) {
        // Try to find the corresponding variable X in the second aggregate
        // by string comparison
        const AstVariable* matchingVarPtr = nullptr;
        visitDepthFirst(agg2, [&](const AstVariable& var) {
            if (var == searchVar) {
                matchingVarPtr = &var;
                return;
            }
        });
        // If the variable occurs in both clauses (a match was found)
        if (matchingVarPtr != nullptr) {
            if (!groundedInAgg1[&searchVar] && groundedInAgg2[matchingVarPtr]) {
                dependent = true;
            }
        }
    });
    return dependent;
}

void AstSemanticCheckerImpl::checkAggregator(const AstAggregator& aggregator) {
    auto& report = tu.getErrorReport();
    auto& program = *tu.getProgram();
    const AstAggregator* inner = nullptr;

    // check for disallowed nested aggregates
    visitDepthFirst(aggregator, [&](const AstAggregator& innerAgg) {
        if (aggregator != innerAgg) {
            inner = &innerAgg;
        }
    });

    if (inner != nullptr) {
        report.addError("Unsupported nested aggregate", inner->getSrcLoc());
    }

    AstClause dummyClauseAggregator;

    visitDepthFirst(program, [&](const AstLiteral& parentLiteral) {
        visitDepthFirst(parentLiteral, [&](const AstAggregator& candidateAggregate) {
            if (candidateAggregate != aggregator) {
                return;
            }
            // Get the literal containing the aggregator and put it into a dummy clause
            // so we can get information about groundedness
            dummyClauseAggregator.addToBody(std::unique_ptr<AstLiteral>(parentLiteral.clone()));
        });
    });

    visitDepthFirst(program, [&](const AstLiteral& parentLiteral) {
        visitDepthFirst(parentLiteral, [&](const AstAggregator& /* otherAggregate */) {
            // Create the other aggregate's dummy clause
            AstClause dummyClauseOther;
            dummyClauseOther.addToBody(std::unique_ptr<AstLiteral>(parentLiteral.clone()));
            // Check dependency between the aggregator and this one
            if (isDependent(dummyClauseAggregator, dummyClauseOther) &&
                    isDependent(dummyClauseOther, dummyClauseAggregator)) {
                report.addError("Mutually dependent aggregate", aggregator.getSrcLoc());
            }
        });
    });

    for (AstLiteral* literal : aggregator.getBodyLiterals()) {
        checkLiteral(*literal);
    }
}

void AstSemanticCheckerImpl::checkArgument(const AstArgument& arg) {
    if (const auto* agg = dynamic_cast<const AstAggregator*>(&arg)) {
        checkAggregator(*agg);
    } else if (const auto* func = dynamic_cast<const AstFunctor*>(&arg)) {
        for (auto arg : func->getArguments()) {
            checkArgument(*arg);
        }
    }
}

static bool isConstantArithExpr(const AstArgument& argument) {
    if (dynamic_cast<const AstNumericConstant*>(&argument) != nullptr) {
        return true;
    }
    if (const auto* functor = dynamic_cast<const AstIntrinsicFunctor*>(&argument)) {
        // Check return type.
        if (!isNumericType(functor->getReturnType())) {
            return false;
        }
        // Check arguments
        return all_of(functor->getArguments(), [](auto* arg) { return isConstantArithExpr(*arg); });
    }
    return false;
}

// TODO (azreika): refactor this (and isConstantArithExpr); confusing name/setup
void AstSemanticCheckerImpl::checkConstant(const AstArgument& argument) {
    if (const auto* var = dynamic_cast<const AstVariable*>(&argument)) {
        report.addError("Variable " + var->getName() + " in fact", var->getSrcLoc());
    } else if (dynamic_cast<const AstUnnamedVariable*>(&argument) != nullptr) {
        report.addError("Underscore in fact", argument.getSrcLoc());
    } else if (dynamic_cast<const AstIntrinsicFunctor*>(&argument) != nullptr) {
        if (!isConstantArithExpr(argument)) {
            report.addError("Function in fact", argument.getSrcLoc());
        }
    } else if (dynamic_cast<const AstUserDefinedFunctor*>(&argument) != nullptr) {
        report.addError("User-defined functor in fact", argument.getSrcLoc());
    } else if (auto* cast = dynamic_cast<const AstTypeCast*>(&argument)) {
        checkConstant(*cast->getValue());
    } else if (dynamic_cast<const AstCounter*>(&argument) != nullptr) {
        report.addError("Counter in fact", argument.getSrcLoc());
    } else if (dynamic_cast<const AstConstant*>(&argument) != nullptr) {
        // this one is fine - type checker will make sure of number and symbol constants
    } else if (auto* ri = dynamic_cast<const AstRecordInit*>(&argument)) {
        for (auto* arg : ri->getArguments()) {
            checkConstant(*arg);
        }
    } else {
        fatal("unsupported argument type: %s", typeid(argument).name());
    }
}

/* Check if facts contain only constants */
void AstSemanticCheckerImpl::checkFact(const AstClause& fact) {
    assert(isFact(fact));

    AstAtom* head = fact.getHead();
    if (head == nullptr) {
        return;  // checked by clause
    }

    AstRelation* rel = getRelation(program, head->getQualifiedName());
    if (rel == nullptr) {
        return;  // checked by clause
    }

    // facts must only contain constants
    for (auto arg : head->getArguments()) {
        checkConstant(*arg);
    }
}

void AstSemanticCheckerImpl::checkClause(const AstClause& clause) {
    // check head atom
    checkAtom(*clause.getHead());

    // check for absence of underscores in head
    if (hasUnnamedVariable(clause.getHead())) {
        report.addError("Underscore in head of rule", clause.getHead()->getSrcLoc());
    }

    // check body literals
    visitDepthFirst(clause, [&](const AstLiteral& lit) {
        if (clause.getHead() == &lit) return;
        checkLiteral(lit);
    });

    // check facts
    if (isFact(clause)) {
        checkFact(clause);
    }

    // check for use-once variables
    std::map<std::string, int> var_count;
    std::map<std::string, const AstVariable*> var_pos;
    visitDepthFirst(clause, [&](const AstVariable& var) {
        var_count[var.getName()]++;
        var_pos[var.getName()] = &var;
    });

    // check for variables only occurring once
    for (const auto& cur : var_count) {
        int numAppearances = cur.second;
        const auto& varName = cur.first;
        const auto& varLocation = var_pos[varName]->getSrcLoc();

        if (varName[0] == '_') {
            assert(varName.size() > 1 && "named variable should not be a single underscore");
            if (numAppearances > 1) {
                report.addWarning("Variable " + varName + " marked as singleton but occurs more than once",
                        varLocation);
            }
        } else if (numAppearances == 1) {
            report.addWarning("Variable " + varName + " only occurs once", varLocation);
        }
    }

    // check execution plan
    if (clause.getExecutionPlan() != nullptr) {
        auto numAtoms = getBodyLiterals<AstAtom>(clause).size();
        for (const auto& cur : clause.getExecutionPlan()->getOrders()) {
            bool isComplete = true;
            auto order = cur.second->getOrder();
            for (unsigned i = 1; i <= order.size(); i++) {
                if (!contains(order, i)) {
                    isComplete = false;
                    break;
                }
            }
            if (order.size() != numAtoms || !isComplete) {
                report.addError("Invalid execution order in plan", cur.second->getSrcLoc());
            }
        }
    }
    // check auto-increment
    if (recursiveClauses.recursive(&clause)) {
        visitDepthFirst(clause, [&](const AstCounter& ctr) {
            report.addError("Auto-increment functor in a recursive rule", ctr.getSrcLoc());
        });
    }
}

void AstSemanticCheckerImpl::checkRelationDeclaration(const AstRelation& relation) {
    const auto& attributes = relation.getAttributes();
    assert(attributes.size() == relation.getArity() && "mismatching attribute size and arity");

    for (size_t i = 0; i < relation.getArity(); i++) {
        AstAttribute* attr = attributes[i];
        AstQualifiedName typeName = attr->getTypeName();

        /* check whether type exists */
        if (!typeEnv.isPrimitiveType(typeName) && (getType(program, typeName) == nullptr)) {
            report.addError("Undefined type in attribute " + attr->getAttributeName() + ":" +
                                    toString(attr->getTypeName()),
                    attr->getSrcLoc());
        }

        /* check whether name occurs more than once */
        for (size_t j = 0; j < i; j++) {
            if (attr->getAttributeName() == attributes[j]->getAttributeName()) {
                report.addError("Doubly defined attribute name " + attr->getAttributeName() + ":" +
                                        toString(attr->getTypeName()),
                        attr->getSrcLoc());
            }
        }
    }
}

void AstSemanticCheckerImpl::checkRelation(const AstRelation& relation) {
    if (relation.getRepresentation() == RelationRepresentation::EQREL) {
        if (relation.getArity() == 2) {
            const auto& attributes = relation.getAttributes();
            assert(attributes.size() == 2 && "mismatching attribute size and arity");
            if (attributes[0]->getTypeName() != attributes[1]->getTypeName()) {
                report.addError("Domains of equivalence relation " + toString(relation.getQualifiedName()) +
                                        " are different",
                        relation.getSrcLoc());
            }
        } else {
            report.addError(
                    "Equivalence relation " + toString(relation.getQualifiedName()) + " is not binary",
                    relation.getSrcLoc());
        }
    }

    // start with declaration
    checkRelationDeclaration(relation);

    // check whether this relation is empty
    if (getClauses(program, relation).empty() && !ioTypes.isInput(&relation) &&
            !relation.hasQualifier(RelationQualifier::SUPPRESSED)) {
        report.addWarning("No rules/facts defined for relation " + toString(relation.getQualifiedName()),
                relation.getSrcLoc());
    }
}

// ----- types --------

void AstSemanticCheckerImpl::checkUnionType(const AstUnionType& type) {
    // check presence of all the element types and that all element types are based off a primitive
    for (const AstQualifiedName& sub : type.getTypes()) {
        if (typeEnv.isPrimitiveType(sub)) {
            continue;
        }
        const AstType* subt = getType(program, sub);
        if (subt == nullptr) {
            report.addError("Undefined type " + toString(sub) + " in definition of union type " +
                                    toString(type.getQualifiedName()),
                    type.getSrcLoc());
        } else if ((dynamic_cast<const AstUnionType*>(subt) == nullptr) &&
                   (dynamic_cast<const AstSubsetType*>(subt) == nullptr)) {
            report.addError("Union type " + toString(type.getQualifiedName()) +
                                    " contains the non-primitive type " + toString(sub),
                    type.getSrcLoc());
        }
    }
}

void AstSemanticCheckerImpl::checkRecordType(const AstRecordType& type) {
    // check proper definition of all field types
    for (const auto& field : type.getFields()) {
        if (!typeEnv.isPrimitiveType(field.type) && (getType(program, field.type) == nullptr)) {
            report.addError(
                    "Undefined type " + toString(field.type) + " in definition of field " + field.name,
                    type.getSrcLoc());
        }
    }

    // check that field names are unique
    auto& fields = type.getFields();
    for (std::size_t i = 0; i < fields.size(); i++) {
        const std::string& cur_name = fields[i].name;
        for (std::size_t j = 0; j < i; j++) {
            if (fields[j].name == cur_name) {
                report.addError("Doubly defined field name " + cur_name + " in definition of type " +
                                        toString(type.getQualifiedName()),
                        type.getSrcLoc());
            }
        }
    }
}

void AstSemanticCheckerImpl::checkType(const AstType& type) {
    if (const auto* u = dynamic_cast<const AstUnionType*>(&type)) {
        checkUnionType(*u);
    } else if (const auto* r = dynamic_cast<const AstRecordType*>(&type)) {
        checkRecordType(*r);
    }
}

// TODO: use graph SCC functions
void AstSemanticCheckerImpl::checkRecursiveUnionTypes() {
    /* Goal: throw an error when unions are cyclically defined */

    // create an edge from each union to its dependents
    Graph<AstQualifiedName> unionTypeGraph = Graph<AstQualifiedName>();
    visitDepthFirst(program, [&](const AstUnionType& ut) {
        for (auto subtype : ut.getTypes()) {
            unionTypeGraph.insert(ut.getQualifiedName(), subtype);
        }
    });

    // helper method to find cycle in the final graph
    std::set<AstQualifiedName> exploredNodes;
    std::function<bool(
            const Graph<AstQualifiedName>&, const AstQualifiedName&, std::vector<AstQualifiedName>&)>
            findCycle;

    // @param   graph union-type dependency graph
    // @param   root next node to explore
    // @param   currPath current path being explored through the graph
    // @return  true if a cycle exists, false otherwise
    findCycle = [&](const Graph<AstQualifiedName>& graph, AstQualifiedName root,
                        std::vector<AstQualifiedName>& currPath) {
        if (exploredNodes.find(root) != exploredNodes.end()) {
            // already checked paths through this node
            return false;
        }

        // exploring paths through this node
        currPath.push_back(root);
        bool cycleFound = false;
        for (auto next : graph.successors(root)) {
            auto pos = std::find(currPath.begin(), currPath.end(), next);
            if (pos != currPath.end()) {
                // cycle found!
                // add to the end to close the cycle
                currPath.push_back(next);
                cycleFound = true;
                break;
            }

            // keep going down this path
            if (findCycle(graph, next, currPath)) {
                cycleFound = true;
                break;
            }
        }

        // done with this node
        exploredNodes.insert(root);

        if (cycleFound) {
            return true;
        } else {
            // no cycle found, bounce back
            currPath.pop_back();
            return false;
        }
    };

    // run the cycle check
    for (const auto& node : unionTypeGraph.vertices()) {
        std::vector<AstQualifiedName> path;
        if (findCycle(unionTypeGraph, node, path)) {
            // pop out the last node of the cycle
            AstQualifiedName cycleStart = path.back();
            path.pop_back();

            // last node is also the first node, so crop out irrelevant nodes from the start
            auto startPos = std::find(path.begin(), path.end(), cycleStart);
            assert(startPos != path.end() && "node should appear twice in cycle");
            path.erase(path.begin(), startPos);

            // get the associated type definition
            auto types = program.getTypes();
            const AstUnionType* typeDefinition = nullptr;
            for (const auto* cur : program.getTypes()) {
                const auto* curUnion = dynamic_cast<const AstUnionType*>(cur);
                if (curUnion == nullptr) {
                    // only care about union types
                    continue;
                }

                // typename must match
                if (curUnion->getQualifiedName() == cycleStart) {
                    typeDefinition = curUnion;
                }
            }
            assert(typeDefinition != nullptr && "typename should have union type definition");

            // add the error
            std::string typeListStr = toString(join(path, ","));
            report.addError(
                    "Cyclic definition of union type(s) {" + typeListStr + "}", typeDefinition->getSrcLoc());
        }
    }
}

void AstSemanticCheckerImpl::checkTypes() {
    /* check each type individually */
    for (const auto* cur : program.getTypes()) {
        checkType(*cur);
    }

    /* check that union types do not mix different primitive types */
    for (const auto* type : program.getTypes()) {
        // We are only interested in unions here.
        if (dynamic_cast<const AstUnionType*>(type) == nullptr) {
            continue;
        }

        const auto& name = type->getQualifiedName();

        const std::set<TypeAttribute>& predefinedTypesInUnion = typeEnvAnalysis.getUnionType(name);

        // Report error (if size == 0, then the union is cyclic)
        if (predefinedTypesInUnion.size() > 1) {
            std::stringstream errorMessage;
            auto toPrimitiveTypeName = [](std::ostream& out, TypeAttribute type) {
                switch (type) {
                    case TypeAttribute::Signed:
                        out << "number";
                        break;
                    case TypeAttribute::Unsigned:
                        out << "unsigned";
                        break;
                    case TypeAttribute::Float:
                        out << "float";
                        break;
                    case TypeAttribute::Symbol:
                        out << "symbol";
                        break;
                    case TypeAttribute::Record:
                        fatal("Invalid type");
                }
            };
            errorMessage << "Union type " << name << " is defined over {"
                         << join(predefinedTypesInUnion, ", ", toPrimitiveTypeName) << "}"
                         << " (multiple primitive types in union)";

            report.addError(errorMessage.str(), type->getSrcLoc());
        }
    }

    /* check that union types are not defined recursively */
    checkRecursiveUnionTypes();
}

void AstSemanticCheckerImpl::checkIO() {
    auto checkIO = [&](const AstIO* directive) {
        auto* r = getRelation(program, directive->getQualifiedName());
        if (r == nullptr) {
            report.addError(
                    "Undefined relation " + toString(directive->getQualifiedName()), directive->getSrcLoc());
        }
    };
    for (const auto* directive : program.getIOs()) {
        checkIO(directive);
    }
}

static const std::vector<SrcLocation> usesInvalidWitness(AstTranslationUnit& tu,
        const std::vector<AstLiteral*>& literals,
        const std::set<std::unique_ptr<AstArgument>>& groundedArguments) {
    // Node-mapper that replaces aggregators with new (unique) variables
    struct M : public AstNodeMapper {
        // Variables introduced to replace aggregators
        mutable std::set<std::string> aggregatorVariables;

        const std::set<std::string>& getAggregatorVariables() {
            return aggregatorVariables;
        }

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            static int numReplaced = 0;
            if (dynamic_cast<AstAggregator*>(node.get()) != nullptr) {
                // Replace the aggregator with a variable
                std::stringstream newVariableName;
                newVariableName << "+aggr_var_" << numReplaced++;

                // Keep track of which variables are bound to aggregators
                aggregatorVariables.insert(newVariableName.str());

                return std::make_unique<AstVariable>(newVariableName.str());
            }
            node->apply(*this);
            return node;
        }
    };

    std::vector<SrcLocation> result;

    // Create two versions of the original clause

    // Clause 1 - will remain equivalent to the original clause in terms of variable groundedness
    auto originalClause = std::make_unique<AstClause>();
    originalClause->setHead(std::make_unique<AstAtom>("*"));

    // Clause 2 - will have aggregators replaced with intrinsically grounded variables
    auto aggregatorlessClause = std::make_unique<AstClause>();
    aggregatorlessClause->setHead(std::make_unique<AstAtom>("*"));

    // Construct both clauses in the same manner to match the original clause
    // Must keep track of the subnode in Clause 1 that each subnode in Clause 2 matches to
    std::map<const AstArgument*, const AstArgument*> identicalSubnodeMap;
    for (const AstLiteral* lit : literals) {
        auto firstClone = std::unique_ptr<AstLiteral>(lit->clone());
        auto secondClone = std::unique_ptr<AstLiteral>(lit->clone());

        // Construct the mapping between equivalent literal subnodes
        std::vector<const AstArgument*> firstCloneArguments;
        visitDepthFirst(*firstClone, [&](const AstArgument& arg) { firstCloneArguments.push_back(&arg); });

        std::vector<const AstArgument*> secondCloneArguments;
        visitDepthFirst(*secondClone, [&](const AstArgument& arg) { secondCloneArguments.push_back(&arg); });

        for (size_t i = 0; i < firstCloneArguments.size(); i++) {
            identicalSubnodeMap[secondCloneArguments[i]] = firstCloneArguments[i];
        }

        // Actually add the literal clones to each clause
        originalClause->addToBody(std::move(firstClone));
        aggregatorlessClause->addToBody(std::move(secondClone));
    }

    // Replace the aggregators in Clause 2 with variables
    M update;
    aggregatorlessClause->apply(update);

    // Create a dummy atom to force certain arguments to be grounded in the aggregatorlessClause
    auto groundingAtomAggregatorless = std::make_unique<AstAtom>("grounding_atom");
    auto groundingAtomOriginal = std::make_unique<AstAtom>("grounding_atom");

    // Force the new aggregator variables to be grounded in the aggregatorless clause
    const std::set<std::string>& aggregatorVariables = update.getAggregatorVariables();
    for (const std::string& str : aggregatorVariables) {
        groundingAtomAggregatorless->addArgument(std::make_unique<AstVariable>(str));
    }

    // Force the given grounded arguments to be grounded in both clauses
    for (const std::unique_ptr<AstArgument>& arg : groundedArguments) {
        groundingAtomAggregatorless->addArgument(std::unique_ptr<AstArgument>(arg->clone()));
        groundingAtomOriginal->addArgument(std::unique_ptr<AstArgument>(arg->clone()));
    }

    aggregatorlessClause->addToBody(std::move(groundingAtomAggregatorless));
    originalClause->addToBody(std::move(groundingAtomOriginal));

    // Compare the grounded analysis of both generated clauses
    // All added arguments in Clause 2 were forced to be grounded, so if an ungrounded argument
    // appears in Clause 2, it must also appear in Clause 1. Consequently, have two cases:
    //   - The argument is also ungrounded in Clause 1 - handled by another check
    //   - The argument is grounded in Clause 1 => the argument was grounded in the
    //     first clause somewhere along the line by an aggregator-body - not allowed!
    std::set<std::unique_ptr<AstArgument>> newlyGroundedArguments;
    auto originalGrounded = getGroundedTerms(tu, *originalClause);
    for (auto&& pair : getGroundedTerms(tu, *aggregatorlessClause)) {
        if (!pair.second && originalGrounded[identicalSubnodeMap[pair.first]]) {
            result.push_back(pair.first->getSrcLoc());
        }

        // Otherwise, it can now be considered grounded
        newlyGroundedArguments.insert(std::unique_ptr<AstArgument>(pair.first->clone()));
    }

    // All previously grounded are still grounded
    for (const std::unique_ptr<AstArgument>& arg : groundedArguments) {
        newlyGroundedArguments.insert(std::unique_ptr<AstArgument>(arg->clone()));
    }

    // Everything on this level is fine, check subaggregators of each literal
    for (const AstLiteral* lit : literals) {
        visitDepthFirst(*lit, [&](const AstAggregator& aggr) {
            // Check recursively if an invalid witness is used
            for (auto&& argloc : usesInvalidWitness(tu, aggr.getBodyLiterals(), newlyGroundedArguments)) {
                result.push_back(argloc);
            }
        });
    }

    return result;
}

void AstSemanticCheckerImpl::checkWitnessProblem() {
    // Visit each clause to check if an invalid aggregator witness is used
    visitDepthFirst(program, [&](const AstClause& clause) {
        // Body literals of the clause to check
        std::vector<AstLiteral*> bodyLiterals = clause.getBodyLiterals();

        // Add in all head variables as new ungrounded body literals
        auto headVariables = std::make_unique<AstAtom>("*");
        visitDepthFirst(*clause.getHead(), [&](const AstVariable& var) {
            headVariables->addArgument(std::unique_ptr<AstVariable>(var.clone()));
        });
        auto headNegation = std::make_unique<AstNegation>(std::move(headVariables));
        bodyLiterals.push_back(headNegation.get());

        // Perform the check
        std::set<std::unique_ptr<AstArgument>> groundedArguments;
        for (auto&& invalidArgument : usesInvalidWitness(tu, bodyLiterals, groundedArguments)) {
            report.addError(
                    "Witness problem: argument grounded by an aggregator's inner scope is used ungrounded in "
                    "outer scope",
                    invalidArgument);
        }
    });
}

/**
 * Find a cycle consisting entirely of inlined relations.
 * If no cycle exists, then an empty vector is returned.
 */
std::vector<AstQualifiedName> findInlineCycle(const PrecedenceGraph& precedenceGraph,
        std::map<const AstRelation*, const AstRelation*>& origins, const AstRelation* current,
        AstRelationSet& unvisited, AstRelationSet& visiting, AstRelationSet& visited) {
    std::vector<AstQualifiedName> result;

    if (current == nullptr) {
        // Not looking at any nodes at the moment, so choose any node from the unvisited list

        if (unvisited.empty()) {
            // Nothing left to visit - so no cycles exist!
            return result;
        }

        // Choose any element from the unvisited set
        current = *unvisited.begin();
        origins[current] = nullptr;

        // Move it to "currently visiting"
        unvisited.erase(current);
        visiting.insert(current);

        // Check if we can find a cycle beginning from this node
        std::vector<AstQualifiedName> subresult =
                findInlineCycle(precedenceGraph, origins, current, unvisited, visiting, visited);

        if (subresult.empty()) {
            // No cycle found, try again from another node
            return findInlineCycle(precedenceGraph, origins, nullptr, unvisited, visiting, visited);
        } else {
            // Cycle found! Return it
            return subresult;
        }
    }

    // Check neighbours
    const AstRelationSet& successors = precedenceGraph.graph().successors(current);
    for (const AstRelation* successor : successors) {
        // Only care about inlined neighbours in the graph
        if (successor->hasQualifier(RelationQualifier::INLINE)) {
            if (visited.find(successor) != visited.end()) {
                // The neighbour has already been visited, so move on
                continue;
            }

            if (visiting.find(successor) != visiting.end()) {
                // Found a cycle!!
                // Construct the cycle in reverse
                while (current != nullptr) {
                    result.push_back(current->getQualifiedName());
                    current = origins[current];
                }
                return result;
            }

            // Node has not been visited yet
            origins[successor] = current;

            // Move from unvisited to visiting
            unvisited.erase(successor);
            visiting.insert(successor);

            // Visit recursively and check if a cycle is formed
            std::vector<AstQualifiedName> subgraphCycle =
                    findInlineCycle(precedenceGraph, origins, successor, unvisited, visiting, visited);

            if (!subgraphCycle.empty()) {
                // Found a cycle!
                return subgraphCycle;
            }
        }
    }

    // Visited all neighbours with no cycle found, so done visiting this node.
    visiting.erase(current);
    visited.insert(current);
    return result;
}

void AstSemanticCheckerImpl::checkInlining() {
    auto isInline = [&](const AstRelation* rel) { return rel->hasQualifier(RelationQualifier::INLINE); };

    // Find all inlined relations
    AstRelationSet inlinedRelations;
    for (const auto& relation : program.getRelations()) {
        if (isInline(relation)) {
            inlinedRelations.insert(relation);
            if (ioTypes.isIO(relation)) {
                report.addError(
                        "IO relation " + toString(relation->getQualifiedName()) + " cannot be inlined",
                        relation->getSrcLoc());
            }
        }
    }

    // Check 1:
    // Let G' be the subgraph of the precedence graph G containing only those nodes
    // which are marked with the inline directive.
    // If G' contains a cycle, then inlining cannot be performed.

    AstRelationSet unvisited;  // nodes that have not been visited yet
    AstRelationSet visiting;   // nodes that we are currently visiting
    AstRelationSet visited;    // nodes that have been completely explored

    // All nodes are initially unvisited
    for (const AstRelation* rel : inlinedRelations) {
        unvisited.insert(rel);
    }

    // Remember the parent node of each visited node to construct the found cycle
    std::map<const AstRelation*, const AstRelation*> origins;

    std::vector<AstQualifiedName> result =
            findInlineCycle(precedenceGraph, origins, nullptr, unvisited, visiting, visited);

    // If the result contains anything, then a cycle was found
    if (!result.empty()) {
        AstRelation* cycleOrigin = getRelation(program, result[result.size() - 1]);

        // Construct the string representation of the cycle
        std::stringstream cycle;
        cycle << "{" << cycleOrigin->getQualifiedName();

        // Print it backwards to preserve the initial cycle order
        for (int i = result.size() - 2; i >= 0; i--) {
            cycle << ", " << result[i];
        }

        cycle << "}";

        report.addError(
                "Cannot inline cyclically dependent relations " + cycle.str(), cycleOrigin->getSrcLoc());
    }

    // Check 2:
    // Cannot use the counter argument ('$') in inlined relations

    // Check if an inlined literal ever takes in a $
    visitDepthFirst(program, [&](const AstAtom& atom) {
        AstRelation* associatedRelation = getRelation(program, atom.getQualifiedName());
        if (associatedRelation != nullptr && isInline(associatedRelation)) {
            visitDepthFirst(atom, [&](const AstArgument& arg) {
                if (dynamic_cast<const AstCounter*>(&arg) != nullptr) {
                    report.addError(
                            "Cannot inline literal containing a counter argument '$'", arg.getSrcLoc());
                }
            });
        }
    });

    // Check if an inlined clause ever contains a $
    for (const AstRelation* rel : inlinedRelations) {
        for (AstClause* clause : getClauses(program, *rel)) {
            visitDepthFirst(*clause, [&](const AstArgument& arg) {
                if (dynamic_cast<const AstCounter*>(&arg) != nullptr) {
                    report.addError(
                            "Cannot inline clause containing a counter argument '$'", arg.getSrcLoc());
                }
            });
        }
    }

    // Check 3:
    // Suppose the relation b is marked with the inline directive, but appears negated
    // in a clause. Then, if b introduces a new variable in its body, we cannot inline
    // the relation b.

    // Find all relations with the inline declarative that introduce new variables in their bodies
    AstRelationSet nonNegatableRelations;
    for (const AstRelation* rel : inlinedRelations) {
        bool foundNonNegatable = false;
        for (const AstClause* clause : getClauses(program, *rel)) {
            // Get the variables in the head
            std::set<std::string> headVariables;
            visitDepthFirst(
                    *clause->getHead(), [&](const AstVariable& var) { headVariables.insert(var.getName()); });

            // Get the variables in the body
            std::set<std::string> bodyVariables;
            visitDepthFirst(clause->getBodyLiterals(),
                    [&](const AstVariable& var) { bodyVariables.insert(var.getName()); });

            // Check if all body variables are in the head
            // Do this separately to the above so only one error is printed per variable
            for (const std::string& var : bodyVariables) {
                if (headVariables.find(var) == headVariables.end()) {
                    nonNegatableRelations.insert(rel);
                    foundNonNegatable = true;
                    break;
                }
            }

            if (foundNonNegatable) {
                break;
            }
        }
    }

    // Check that these relations never appear negated
    visitDepthFirst(program, [&](const AstNegation& neg) {
        if (auto atom = dynamic_cast<AstAtom*>(neg.getLiteral())) {
            AstRelation* associatedRelation = getRelation(program, atom->getQualifiedName());
            if (associatedRelation != nullptr &&
                    nonNegatableRelations.find(associatedRelation) != nonNegatableRelations.end()) {
                report.addError(
                        "Cannot inline negated relation which may introduce new variables", neg.getSrcLoc());
            }
        }
    });

    // Check 4:
    // Don't support inlining atoms within aggregators at this point.

    // Reasoning: Suppose we have an aggregator like `max X: a(X)`, where `a` is inlined to `a1` and `a2`.
    // Then, `max X: a(X)` will become `max( max X: a1(X),  max X: a2(X) )`. Suppose further that a(X) has
    // values X where it is true, while a2(X) does not. Then, the produced argument
    // `max( max X: a1(X),  max X: a2(X) )` will not return anything (as one of its arguments fails), while
    // `max X: a(X)` will.
    // Can work around this with emptiness checks (e.g. `!a1(_), ... ; !a2(_), ... ; ...`)

    // This corner case prevents generalising aggregator inlining with the current set up.

    visitDepthFirst(program, [&](const AstAggregator& aggr) {
        visitDepthFirst(aggr, [&](const AstAtom& subatom) {
            const AstRelation* rel = getRelation(program, subatom.getQualifiedName());
            if (rel != nullptr && isInline(rel)) {
                report.addError("Cannot inline relations that appear in aggregator", subatom.getSrcLoc());
            }
        });
    });

    // Check 5:
    // Suppose a relation `a` is inlined, appears negated in a clause, and contains a
    // (possibly nested) unnamed variable in its arguments. Then, the atom can't be
    // inlined, as unnamed variables are named during inlining (since they may appear
    // multiple times in an inlined-clause's body) => ungroundedness!

    // Exception: It's fine if the unnamed variable appears in a nested aggregator, as
    // the entire aggregator will automatically be grounded.

    // TODO (azreika): special case where all rules defined for `a` use the
    // underscored-argument exactly once: can workaround by remapping the variable
    // back to an underscore - involves changes to the actual inlining algo, though

    // Returns the pair (isValid, lastSrcLoc) where:
    //  - isValid is true if and only if the node contains an invalid underscore, and
    //  - lastSrcLoc is the source location of the last visited node
    std::function<std::pair<bool, SrcLocation>(const AstNode*)> checkInvalidUnderscore =
            [&](const AstNode* node) {
                if (dynamic_cast<const AstUnnamedVariable*>(node) != nullptr) {
                    // Found an invalid underscore
                    return std::make_pair(true, node->getSrcLoc());
                } else if (dynamic_cast<const AstAggregator*>(node) != nullptr) {
                    // Don't care about underscores within aggregators
                    return std::make_pair(false, node->getSrcLoc());
                }

                // Check if any children nodes use invalid underscores
                for (const AstNode* child : node->getChildNodes()) {
                    std::pair<bool, SrcLocation> childStatus = checkInvalidUnderscore(child);
                    if (childStatus.first) {
                        // Found an invalid underscore
                        return childStatus;
                    }
                }

                return std::make_pair(false, node->getSrcLoc());
            };

    // Perform the check
    visitDepthFirst(program, [&](const AstNegation& negation) {
        if (const AstAtom* associatedAtom = dynamic_cast<const AstAtom*>(negation.getLiteral())) {
            const AstRelation* associatedRelation = getRelation(program, associatedAtom->getQualifiedName());
            if (associatedRelation != nullptr && isInline(associatedRelation)) {
                std::pair<bool, SrcLocation> atomStatus = checkInvalidUnderscore(associatedAtom);
                if (atomStatus.first) {
                    report.addError(
                            "Cannot inline negated atom containing an unnamed variable unless the variable "
                            "is "
                            "within an aggregator",
                            atomStatus.second);
                }
            }
        }
    });
}

// Check that type and relation names are disjoint sets.
void AstSemanticCheckerImpl::checkNamespaces() {
    std::map<std::string, SrcLocation> names;

    // Find all names and report redeclarations as we go.
    for (const auto& type : program.getTypes()) {
        const std::string name = toString(type->getQualifiedName());
        if (names.count(name) != 0u) {
            report.addError("Name clash on type " + name, type->getSrcLoc());
        } else {
            names[name] = type->getSrcLoc();
        }
    }

    for (const auto& rel : program.getRelations()) {
        const std::string name = toString(rel->getQualifiedName());
        if (names.count(name) != 0u) {
            report.addError("Name clash on relation " + name, rel->getSrcLoc());
        } else {
            names[name] = rel->getSrcLoc();
        }
    }
}

bool AstExecutionPlanChecker::transform(AstTranslationUnit& translationUnit) {
    auto* relationSchedule = translationUnit.getAnalysis<RelationSchedule>();
    auto* recursiveClauses = translationUnit.getAnalysis<RecursiveClauses>();

    for (const RelationScheduleStep& step : relationSchedule->schedule()) {
        const std::set<const AstRelation*>& scc = step.computed();
        for (const AstRelation* rel : scc) {
            for (const AstClause* clause : getClauses(*translationUnit.getProgram(), *rel)) {
                if (!recursiveClauses->recursive(clause)) {
                    continue;
                }
                if (clause->getExecutionPlan() == nullptr) {
                    continue;
                }
                int version = 0;
                for (const auto* atom : getBodyLiterals<AstAtom>(*clause)) {
                    if (scc.count(getAtomRelation(atom, translationUnit.getProgram())) != 0u) {
                        version++;
                    }
                }
                int maxVersion = -1;
                for (auto const& cur : clause->getExecutionPlan()->getOrders()) {
                    maxVersion = std::max(cur.first, maxVersion);
                }

                if (version <= maxVersion) {
                    for (const auto& cur : clause->getExecutionPlan()->getOrders()) {
                        if (cur.first >= version) {
                            translationUnit.getErrorReport().addDiagnostic(Diagnostic(Diagnostic::ERROR,
                                    DiagnosticMessage(
                                            "execution plan for version " + std::to_string(cur.first),
                                            cur.second->getSrcLoc()),
                                    {DiagnosticMessage("only versions 0.." + std::to_string(version - 1) +
                                                       " permitted")}));
                        }
                    }
                }
            }
        }
    }
    return false;
}

void GroundedTermsChecker::verify(AstTranslationUnit& translationUnit) {
    auto&& program = *translationUnit.getProgram();
    auto&& report = translationUnit.getErrorReport();

    // -- check grounded variables and records --
    visitDepthFirst(program.getClauses(), [&](const AstClause& clause) {
        if (isFact(clause)) return;  // only interested in rules

        auto isGrounded = getGroundedTerms(translationUnit, clause);

        std::set<std::string> reportedVars;
        // all terms in head need to be grounded
        for (auto&& cur : getVariables(clause)) {
            if (!isGrounded[cur] && reportedVars.insert(cur->getName()).second) {
                report.addError("Ungrounded variable " + cur->getName(), cur->getSrcLoc());
            }
        }

        // all records need to be grounded
        for (auto&& cur : getRecords(clause)) {
            if (!isGrounded[cur]) {
                report.addError("Ungrounded record", cur->getSrcLoc());
            }
        }
    });
}

}  // end of namespace souffle
