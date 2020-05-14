/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstUtils.cpp
 *
 * A collection of utilities operating on AST constructs.
 *
 ***********************************************************************/

#include "AstUtils.h"
#include "AstArgument.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstType.h"
#include "AstVisitor.h"

namespace souffle {

std::string pprint(const AstNode& node) {
    return toString(node);
}

std::vector<const AstVariable*> getVariables(const AstNode& root) {
    // simply collect the list of all variables by visiting all variables
    std::vector<const AstVariable*> vars;
    visitDepthFirst(root, [&](const AstVariable& var) { vars.push_back(&var); });
    return vars;
}

std::vector<const AstRecordInit*> getRecords(const AstNode& root) {
    // simply collect the list of all records by visiting all records
    std::vector<const AstRecordInit*> recs;
    visitDepthFirst(root, [&](const AstRecordInit& rec) { recs.push_back(&rec); });
    return recs;
}

std::vector<AstClause*> getClauses(const AstProgram& program, const AstQualifiedName& relationName) {
    std::vector<AstClause*> clauses;
    for (AstClause* clause : program.getClauses()) {
        if (clause->getHead()->getQualifiedName() == relationName) {
            clauses.push_back(clause);
        }
    }
    return clauses;
}

std::vector<AstClause*> getClauses(const AstProgram& program, const AstRelation& rel) {
    return getClauses(program, rel.getQualifiedName());
}

AstRelation* getRelation(const AstProgram& program, const AstQualifiedName& name) {
    return getIf(program.getRelations(), [&](const AstRelation* r) { return r->getQualifiedName() == name; });
}

const AstType* getType(const AstProgram& program, const AstQualifiedName& name) {
    return getIf(program.getTypes(), [&](const AstType* t) { return t->getQualifiedName() == name; });
}

const AstFunctorDeclaration* getFunctorDeclaration(const AstProgram& program, const std::string& name) {
    return getIf(program.getFunctorDeclarations(),
            [&](const AstFunctorDeclaration* f) { return f->getName() == name; });
}

void removeRelationClauses(AstProgram& program, const AstQualifiedName& name) {
    for (const auto* clause : getClauses(program, name)) {
        program.removeClause(clause);
    }
}

const AstRelation* getAtomRelation(const AstAtom* atom, const AstProgram* program) {
    return getRelation(*program, atom->getQualifiedName());
}

const AstRelation* getHeadRelation(const AstClause* clause, const AstProgram* program) {
    return getAtomRelation(clause->getHead(), program);
}

std::set<const AstRelation*> getBodyRelations(const AstClause* clause, const AstProgram* program) {
    std::set<const AstRelation*> bodyRelations;
    for (const auto& lit : clause->getBodyLiterals()) {
        visitDepthFirst(
                *lit, [&](const AstAtom& atom) { bodyRelations.insert(getAtomRelation(&atom, program)); });
    }
    for (const auto& arg : clause->getHead()->getArguments()) {
        visitDepthFirst(
                *arg, [&](const AstAtom& atom) { bodyRelations.insert(getAtomRelation(&atom, program)); });
    }
    return bodyRelations;
}

size_t getClauseNum(const AstProgram* program, const AstClause* clause) {
    // TODO (azreika): This number might change between the provenance transformer and the AST->RAM
    // translation. Might need a better way to assign IDs to clauses... (see PR #1288).
    const AstRelation* rel = getRelation(*program, clause->getHead()->getQualifiedName());
    assert(rel != nullptr && "clause relation does not exist");

    size_t clauseNum = 1;
    for (const auto* cur : getClauses(*program, *rel)) {
        bool isFact = cur->getBodyLiterals().empty();
        if (cur == clause) {
            return isFact ? 0 : clauseNum;
        }

        if (!isFact) {
            clauseNum++;
        }
    }

    fatal("clause does not exist");
}

bool hasClauseWithNegatedRelation(const AstRelation* relation, const AstRelation* negRelation,
        const AstProgram* program, const AstLiteral*& foundLiteral) {
    for (const AstClause* cl : getClauses(*program, *relation)) {
        for (const auto* neg : getBodyLiterals<AstNegation>(*cl)) {
            auto atom = neg->getAtom();
            if (atom && negRelation == getAtomRelation(atom, program)) {
                foundLiteral = neg;
                return true;
            }
        }
    }
    return false;
}

bool hasClauseWithAggregatedRelation(const AstRelation* relation, const AstRelation* aggRelation,
        const AstProgram* program, const AstLiteral*& foundLiteral) {
    for (const AstClause* cl : getClauses(*program, *relation)) {
        bool hasAgg = false;
        visitDepthFirst(*cl, [&](const AstAggregator& cur) {
            visitDepthFirst(cur, [&](const AstAtom& atom) {
                if (aggRelation == getAtomRelation(&atom, program)) {
                    foundLiteral = &atom;
                    hasAgg = true;
                }
            });
        });
        if (hasAgg) {
            return true;
        }
    }
    return false;
}

bool isRecursiveClause(const AstClause& clause) {
    AstQualifiedName relationName = clause.getHead()->getQualifiedName();
    bool recursive = false;
    visitDepthFirst(clause.getBodyLiterals(), [&](const AstAtom& atom) {
        if (atom.getQualifiedName() == relationName) {
            recursive = true;
        }
    });
    return recursive;
}

bool isFact(const AstClause& clause) {
    // there must be a head
    if (clause.getHead() == nullptr) {
        return false;
    }
    // there must not be any body clauses
    if (!clause.getBodyLiterals().empty()) {
        return false;
    }

    // and there are no aggregates
    bool hasAggregates = false;
    visitDepthFirst(*clause.getHead(), [&](const AstAggregator&) { hasAggregates = true; });
    return !hasAggregates;
}

bool isRule(const AstClause& clause) {
    return (clause.getHead() != nullptr) && !isFact(clause);
}

AstClause* cloneHead(const AstClause* clause) {
    auto* clone = new AstClause();
    clone->setSrcLoc(clause->getSrcLoc());
    clone->setHead(std::unique_ptr<AstAtom>(clause->getHead()->clone()));
    if (clause->getExecutionPlan() != nullptr) {
        clone->setExecutionPlan(std::unique_ptr<AstExecutionPlan>(clause->getExecutionPlan()->clone()));
    }
    return clone;
}

AstClause* reorderAtoms(const AstClause* clause, const std::vector<unsigned int>& newOrder) {
    // Find all atom positions
    std::vector<unsigned int> atomPositions;
    std::vector<AstLiteral*> bodyLiterals = clause->getBodyLiterals();
    for (unsigned int i = 0; i < bodyLiterals.size(); i++) {
        if (dynamic_cast<AstAtom*>(bodyLiterals[i]) != nullptr) {
            atomPositions.push_back(i);
        }
    }

    // Validate given order
    assert(newOrder.size() == atomPositions.size());
    std::vector<unsigned int> nopOrder;
    for (unsigned int i = 0; i < atomPositions.size(); i++) {
        nopOrder.push_back(i);
    }
    assert(std::is_permutation(nopOrder.begin(), nopOrder.end(), newOrder.begin()));

    // Create a new clause with the given atom order, leaving the rest unchanged
    AstClause* newClause = cloneHead(clause);
    unsigned int currentAtom = 0;
    for (unsigned int currentLiteral = 0; currentLiteral < bodyLiterals.size(); currentLiteral++) {
        AstLiteral* literalToAdd = bodyLiterals[currentLiteral];
        if (dynamic_cast<AstAtom*>(literalToAdd) != nullptr) {
            // Atoms should be reordered
            literalToAdd = bodyLiterals[atomPositions[newOrder[currentAtom++]]];
        }
        newClause->addToBody(std::unique_ptr<AstLiteral>(literalToAdd->clone()));
    }

    return newClause;
}

void negateConstraint(AstConstraint* constraint) {
    assert(nullptr != dynamic_cast<AstConstraint*>(constraint) && "not a constraint object");
    if (auto* bcstr = dynamic_cast<AstBooleanConstraint*>(constraint)) {
        bcstr->set(!bcstr->isTrue());
    } else if (auto* cstr = dynamic_cast<AstBinaryConstraint*>(constraint)) {
        cstr->setOperator(souffle::negatedConstraintOp(cstr->getOperator()));
    } else {
        fatal("Unknown ast-constraint type");
    }
}

namespace {

// FIXME: this isn't safe/correct, we should use arbitrary precision arith.
// TODO: add arith ops interpreter
std::optional<double> constantEvalNumeric(const AstArgument& a) {
    if (auto a_num = dynamic_cast<const AstNumericConstant*>(&a)) {
        auto ty = a_num->getType();
        switch (ty ? *ty : AstNumericConstant::Type::Int) {
            case AstNumericConstant::Type::Float:
                return RamFloatFromString(a_num->getConstant());
            case AstNumericConstant::Type::Int:
                return RamSignedFromString(a_num->getConstant());
            case AstNumericConstant::Type::Uint:
                return RamUnsignedFromString(a_num->getConstant());
        }
    }

    return {};
}

bool isConstantExpr(const AstArgument& a) {
    if (isA<AstAggregator>(a)) return false;  // conservatively claim it isn't (it might be, in edge cases)
    if (isA<AstVariable>(a)) return false;    // it might be, but we can't prove it here
    if (isA<AstUserDefinedFunctor>(a)) return false;  // AFAIK we don't require that these are pure
    if (isA<AstCounter>(a)) return false;             // impure built-in functor

    bool constExpr = true;
    visitDepthFirst(a, [&](const AstArgument& a) { constExpr &= isConstantExpr(a); });
    return constExpr;  // all sub exprs must also be constant exprs
}

// HACK: `NE` case for when we can prove they're not equal, but don't have an impl proving `LT`/`GT`.
enum class Comp { EQ, NE, LT, GT };
std::optional<Comp> constantEvalCompare(const AstArgument& a, const AstArgument& b) {
    // FIXME: Breaks NaN checks of the form (`x = x`).
    //        We'd probably be better off adding a special literal form instead (e.g. `isNaN`).
    if (a == b) return Comp::EQ;  // structural equality implies value quality (given same context)

    auto a_num = constantEvalNumeric(a);
    auto b_num = constantEvalNumeric(b);
    if (a_num && b_num) {  // both const numeric exprs -> can give definite answer
        if (a_num < b_num) return Comp::LT;
        if (a_num > b_num) return Comp::GT;
        return Comp::EQ;
    }

    return {};  // no idea, unhandled form or non-const expr
}

}  // namespace

std::optional<bool> constantEval(const AstConstraint& constraint) {
    if (auto bc = dynamic_cast<const AstBooleanConstraint*>(&constraint)) return bc->isTrue();

    if (auto bin = dynamic_cast<const AstBinaryConstraint*>(&constraint)) {
        // if either side is an anonymous var then we're trivially equal (morally speaking)
        // (we can pretend the unnamed var is a value that satisfies the constraint)
        if (isA<AstUnnamedVariable>(*bin->getLHS())) return true;
        if (isA<AstUnnamedVariable>(*bin->getRHS())) return true;

        auto comp = constantEvalCompare(*bin->getLHS(), *bin->getRHS());
        if (!comp) return {};  // couldn't evaluate

        switch (bin->getOperator()) {
#define COMP(op, expr)              \
    case BinaryConstraintOp::op:    \
    case BinaryConstraintOp::F##op: \
    case BinaryConstraintOp::U##op: \
    case BinaryConstraintOp::S##op: \
        return expr;

            COMP(LT, comp == Comp::LT)
            COMP(GT, comp == Comp::GT)
            COMP(LE, comp == Comp::LT || comp == Comp::EQ)
            COMP(GE, comp == Comp::GT || comp == Comp::EQ)
#undef COMP

            case BinaryConstraintOp::EQ:
            case BinaryConstraintOp::FEQ:
                return comp == Comp::EQ;

            case BinaryConstraintOp::NE:
            case BinaryConstraintOp::FNE:
                return comp != Comp::EQ;

            case BinaryConstraintOp::MATCH:
            case BinaryConstraintOp::CONTAINS:
            case BinaryConstraintOp::NOT_MATCH:
            case BinaryConstraintOp::NOT_CONTAINS:
                return {};  // TODO: implement
        }

        UNREACHABLE_BAD_CASE_ANALYSIS
    }

    return {};
}

}  // end of namespace souffle
