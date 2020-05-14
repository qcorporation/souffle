/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ResolveAliasesTransformer.cpp
 *
 * Define classes and functionality related to the ResolveAliases
 * transformer.
 *
 ***********************************************************************/

#include "AstArgument.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstTransforms.h"
#include "AstTranslationUnit.h"
#include "AstUtils.h"
#include "AstVisitor.h"
#include "BinaryConstraintOps.h"
#include "Util.h"
#include <cassert>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace souffle {

namespace {

/**
 * A utility class for the unification process required to eliminate aliases.
 * A substitution maps variables to terms and can be applied as a transformation
 * to AstArguments.
 */
struct Substitution : public AstNodeMapper {
    // map type used for internally storing var->term mappings
    //      - note: variables are identified by their names
    using map_t = std::map<std::string, std::unique_ptr<AstArgument>>;

    // the mapping of variables to terms
    map_t varToTerm;

    Substitution() = default;
    Substitution(const Substitution&) = default;
    Substitution(Substitution&&) = default;
    Substitution& operator=(const Substitution&) = default;
    Substitution& operator=(Substitution&&) = default;

    Substitution(const std::string& var, const AstArgument* arg) {
        varToTerm.insert(std::make_pair(var, std::unique_ptr<AstArgument>(arg->clone())));
    }

    using AstNodeMapper::operator();

    /**
     * Applies this substitution to the given argument and returns a pointer
     * to the modified argument.
     *
     * @param node the node to be transformed
     * @return a pointer to the modified or replaced node
     */
    std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const {
        /// see whether it is a variable to be substituted
        if (auto var = dynamic_cast<AstVariable*>(node.get())) {
            auto pos = varToTerm.find(var->getName());
            if (pos != varToTerm.end()) return clone(pos->second);
        }

        // otherwise, apply the mapper recursively
        node->apply(*this);
        return node;
    }

    /**
     * Appends the given substitution s to this substitution t such that the
     * result t' is s composed with t (s o t).
     * i.e.,
     *      - if t(x) = y, then t'(x) = s(y)
     *      - if s(x) = y, and x is not mapped by t, then t'(x) = y
     */
    void append(const Substitution& sub) {
        // apply substitution on the rhs of all current mappings
        for (auto& pair : varToTerm) {
            pair.second = sub(std::move(pair.second));
        }

        // append unseen variables to the end
        for (const auto& pair : sub.varToTerm) {
            if (varToTerm.find(pair.first) == varToTerm.end()) {
                // not seen yet, add it in
                varToTerm.insert(
                        std::make_pair(pair.first, std::unique_ptr<AstArgument>(pair.second->clone())));
            }
        }
    }

    /** A print function (for debugging) */
    void print(std::ostream& out) const {
        out << "{"
            << join(varToTerm, ",",
                       [](std::ostream& out,
                               const std::pair<const std::string, std::unique_ptr<AstArgument>>& cur) {
                           out << cur.first << " -> " << *cur.second;
                       })
            << "}";
    }

    friend std::ostream& operator<<(std::ostream& out, const Substitution& s) __attribute__((unused)) {
        s.print(out);
        return out;
    }
};

/**
 * An equality constraint between two AstArguments utilised by the unification
 * algorithm required by the alias resolution.
 */
class Equation {
public:
    // the two terms to be equivalent
    std::unique_ptr<AstArgument> lhs;
    std::unique_ptr<AstArgument> rhs;

    Equation(const AstArgument& lhs, const AstArgument& rhs) : Equation(&lhs, &rhs) {}
    Equation(const AstArgument* lhs, const AstArgument* rhs) : lhs(clone(lhs)), rhs(clone(rhs)) {}
    Equation(const Equation& x) : lhs(clone(x.lhs)), rhs(clone(x.rhs)) {}
    Equation(Equation&&) = default;

    /**
     * Applies the given substitution to both sides of the equation.
     */
    void apply(const Substitution& sub) {
        lhs = sub(std::move(lhs));
        rhs = sub(std::move(rhs));
    }

    /**
     * Enables equations to be printed (for debugging)
     */
    void print(std::ostream& out) const {
        out << *lhs << " = " << *rhs;
    }

    friend std::ostream& operator<<(std::ostream& out, const Equation& e) __attribute__((unused)) {
        e.print(out);
        return out;
    }
};

bool occurs(const AstArgument& a, const AstArgument& b) {
    bool res = false;
    visitDepthFirst(b, [&](const AstArgument& arg) { res = (res || (arg == a)); });
    return res;
}

void literalScopes(AstNode& root, std::vector<AstLiteral*>& currLevel, std::vector<AstNode*>& subScopes) {
    for (auto childC : root.getChildNodes()) {
        auto child = const_cast<AstNode*>(childC);

        if (auto neg = dynamic_cast<AstNegation*>(child)) {
            subScopes.push_back(neg);
            continue;
        }

        if (auto agg = dynamic_cast<AstAggregator*>(child)) {
            subScopes.push_back(agg);
            continue;
        }

        if (auto lit = dynamic_cast<AstLiteral*>(child)) {
            currLevel.push_back(lit);
        }

        literalScopes(*child, currLevel, subScopes);
    }
}

Substitution resolveEquations(const std::set<std::string>& groundedVars, std::vector<Equation> equations) {
    Substitution substitution;

    // a utility for processing newly identified mappings
    auto newMapping = [&](const std::string& var, const AstArgument* term) {
        // found a new substitution
        Substitution newMapping(var, term);

        // apply substitution to all remaining equations
        for (auto& equation : equations) {
            equation.apply(newMapping);
        }

        // add mapping v -> t to substitution
        substitution.append(newMapping);
    };

    while (!equations.empty()) {
        // get next equation to compute
        Equation equation = std::move(equations.back());
        equations.pop_back();

        // shortcuts for left/right
        const AstArgument& lhs = *equation.lhs;
        const AstArgument& rhs = *equation.rhs;

        // #1:  t = t   => skip
        if (lhs == rhs) {
            continue;
        }

        // #2:  [..] = [..]  => decompose
        auto lhs_rec = dynamic_cast<const AstRecordInit*>(&lhs);
        auto rhs_rec = dynamic_cast<const AstRecordInit*>(&rhs);
        if (lhs_rec && rhs_rec) {
            auto&& lhs_args = lhs_rec->getArguments();
            auto&& rhs_args = rhs_rec->getArguments();
            assert(lhs_args.size() == rhs_args.size() && "Record lengths not equal");

            // create new equalities
            for (size_t i = 0; i < lhs_args.size(); i++) {
                equations.push_back(Equation(lhs_args[i], rhs_args[i]));
            }

            continue;
        }

        // #3:  v = w    => add mapping
        auto lhs_var = dynamic_cast<const AstVariable*>(&lhs);
        auto rhs_var = dynamic_cast<const AstVariable*>(&rhs);
        if (lhs_var && rhs_var) {
            newMapping(lhs_var->getName(), &rhs);
            continue;
        }

        // #4:  t = v   => swap
        if (rhs_var) {
            equations.push_back(Equation(rhs, lhs));
            continue;
        }

        // #5: v = t    => attempt substitution with `t`
        if (lhs_var) {
            // #5a:  v occurs in t   => skip
            if (occurs(*lhs_var, rhs)) continue;

            // #5b:  t is a record   => add mapping
            if (isA<AstRecordInit>(rhs)) {
                newMapping(lhs_var->getName(), &rhs);
                continue;
            }

            // #5c:  v is already grounded   => skip
            if (contains(groundedVars, lhs_var->getName())) continue;

            // #5d:  v is a temp var, add new mapping
            newMapping(lhs_var->getName(), &rhs);
        }
    }

    return substitution;
}

void simplifyAliases(AstNode& node, std::set<std::string> groundedVars = {}) {
    std::vector<AstLiteral*> lits;
    std::vector<AstNode*> subScopes;
    literalScopes(node, lits, subScopes);

    if (auto clause = dynamic_cast<AstClause*>(&node)) {
        // remove the clause head atom, it doesn't ground anything.
        lits = filterNot(lits, [&](auto l) { return l == clause->getHead(); });
    }

    std::vector<Equation> equations;

    for (auto&& lit : lits) {
        // find all variables appearing as functorless arguments in grounding atoms
        // these variables are the source of groundedness
        // e.g. a(y) :- b(x), y = x + 1. -- y is only grounded because x appears in b(x)
        if (auto atom = dynamic_cast<AstAtom*>(lit)) {
            for (auto&& arg : atom->getArguments()) {
                if (auto var = dynamic_cast<AstVariable*>(arg)) {
                    groundedVars.insert(var->getName());
                }
            }
        }

        if (auto constraint = dynamic_cast<AstBinaryConstraint*>(lit)) {
            if (isEqConstraint(constraint->getOperator())) {
                equations.push_back(Equation(constraint->getLHS(), constraint->getRHS()));
            }
        }
    }

    // FIXME: this does a fair bit of duplicated work.
    auto subs = resolveEquations(groundedVars, std::move(equations));
    node.apply(subs);

    lits.clear();
    subScopes.clear();
    literalScopes(node, lits, subScopes);
    for (auto&& sub : subScopes) {
        simplifyAliases(*sub, groundedVars);
    }
}

}  // namespace

std::unique_ptr<AstClause> ResolveAliasesTransformer::resolveAliases(const AstClause& clause) {
    auto cpy = clone(&clause);
    simplifyAliases(*cpy);
    return cpy;
}

std::unique_ptr<AstClause> ResolveAliasesTransformer::removeTrivialEquality(const AstClause& clause) {
    std::unique_ptr<AstClause> res(cloneHead(&clause));

    // add all literals, except filtering out t = t constraints
    for (AstLiteral* literal : clause.getBodyLiterals()) {
        if (auto* constraint = dynamic_cast<AstBinaryConstraint*>(literal)) {
            // TODO: don't filter out `FEQ` constraints, since `x = x` can fail when `x` is a NaN
            if (isEqConstraint(constraint->getOperator())) {
                if (*constraint->getLHS() == *constraint->getRHS()) {
                    continue;  // skip this one
                }
            }
        }

        res->addToBody(std::unique_ptr<AstLiteral>(literal->clone()));
    }

    // done
    return res;
}

std::unique_ptr<AstClause> ResolveAliasesTransformer::removeComplexTermsInAtoms(const AstClause& clause) {
    std::unique_ptr<AstClause> res(clause.clone());

    // get list of atoms
    std::vector<AstAtom*> atoms = getBodyLiterals<AstAtom>(*res);

    // find all functors in atoms
    std::vector<const AstArgument*> terms;
    for (const AstAtom* atom : atoms) {
        for (const AstArgument* arg : atom->getArguments()) {
            // ignore if not a functor
            if (dynamic_cast<const AstFunctor*>(arg) == nullptr) {
                continue;
            }

            // add this functor if not seen yet
            if (!any_of(terms, [&](const AstArgument* cur) { return *cur == *arg; })) {
                terms.push_back(arg);
            }
        }
    }

    // substitute them with new variables (a real map would compare pointers)
    using substitution_map =
            std::vector<std::pair<std::unique_ptr<AstArgument>, std::unique_ptr<AstVariable>>>;
    substitution_map termToVar;

    int varCounter = 0;
    for (const AstArgument* arg : terms) {
        // create a new mapping for this term
        auto term = std::unique_ptr<AstArgument>(arg->clone());
        auto newVariable = std::make_unique<AstVariable>(" _tmp_" + toString(varCounter++));
        termToVar.push_back(std::make_pair(std::move(term), std::move(newVariable)));
    }

    // apply mapping to replace the terms with the variables
    struct Update : public AstNodeMapper {
        const substitution_map& map;

        Update(const substitution_map& map) : map(map) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            // check whether node needs to be replaced
            for (const auto& pair : map) {
                auto& term = pair.first;
                auto& variable = pair.second;

                if (*term == *node) {
                    return std::unique_ptr<AstNode>(variable->clone());
                }
            }

            // continue recursively
            node->apply(*this);
            return node;
        }
    };

    // update atoms
    Update update(termToVar);
    for (AstAtom* atom : atoms) {
        atom->apply(update);
    }

    // add the necessary variable constraints to the clause
    for (const auto& pair : termToVar) {
        auto& term = pair.first;
        auto& variable = pair.second;

        res->addToBody(std::make_unique<AstBinaryConstraint>(BinaryConstraintOp::EQ,
                std::unique_ptr<AstArgument>(variable->clone()),
                std::unique_ptr<AstArgument>(term->clone())));
    }

    return res;
}

bool ResolveAliasesTransformer::transform(AstTranslationUnit& translationUnit) {
    bool changed = false;
    AstProgram& program = *translationUnit.getProgram();

    // get all clauses
    std::vector<const AstClause*> clauses;
    visitDepthFirst(program, [&](const AstRelation& rel) {
        for (const auto& clause : getClauses(program, rel)) {
            clauses.push_back(clause);
        }
    });

    // clean all clauses
    for (const AstClause* clause : clauses) {
        // -- Step 1 --
        // get rid of aliases
        std::unique_ptr<AstClause> noAlias = resolveAliases(*clause);

        // clean up equalities
        std::unique_ptr<AstClause> cleaned = removeTrivialEquality(*noAlias);

        // -- Step 2 --
        // restore simple terms in atoms
        std::unique_ptr<AstClause> normalised = removeComplexTermsInAtoms(*cleaned);

        // swap if changed
        if (*normalised != *clause) {
            changed = true;
            program.removeClause(clause);
            program.addClause(std::move(normalised));
        }
    }

    return changed;
}

}  // namespace souffle
