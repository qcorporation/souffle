/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InlineRelationsTransformer.cpp
 *
 * Define classes and functionality related to inlining.
 *
 ***********************************************************************/

#include "AggregateOp.h"
#include "AstArgument.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstQualifiedName.h"
#include "AstRelation.h"
#include "AstTransforms.h"
#include "AstTranslationUnit.h"
#include "AstUtils.h"
#include "AstVisitor.h"
#include "BinaryConstraintOps.h"
#include "FunctorOps.h"
#include "Util.h"
#include <cassert>
#include <cstddef>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

// FIXME: The following are defined in modern utilities. Remove when we rebase on upstream.
template <typename A>
using Own = std::unique_ptr<A>;

template <typename A>
using VecOwn = std::vector<Own<A>>;

template <typename A, typename B = A, typename... Args>
Own<A> mk(Args&&... xs) {
    return Own<A>(new B(std::forward<Args>(xs)...));
}

template <typename A, typename B>
using copy_const_t = std::conditional_t<std::is_const_v<A>, const B, B>;

/**
 * Helpers for `dynamic_cast`ing without having to specify redundant type qualifiers.
 * e.g. `as<AstLiteral>(p)` instead of `dynamic_cast<const AstLiteral*>(p.get())`.
 */
template <typename B, typename A>
auto as(A* x) {
    static_assert(std::is_base_of_v<A, B>,
            "`as<B, A>` does not allow cross-type dyn casts. "
            "(i.e. `as<B, A>` where `B <: A` is not true.) "
            "Such a cast is likely a mistake or typo.");
    return dynamic_cast<copy_const_t<A, B>*>(x);
}

template <typename B, typename A>
std::enable_if_t<std::is_base_of_v<A, B>, copy_const_t<A, B>*> as(A& x) {
    return as<B>(&x);
}

template <typename B, typename A>
B* as(const Own<A>& x) {
    return as<B>(x.get());
}

/**
 * Checks if the object of type Source can be casted to type Destination.
 */
template <typename Destination, typename Source>
bool isA(Source&& src) {
    return as<Destination>(std::forward<Source>(src)) != nullptr;
}

bool InlineRelationsTransformer::transform(AstTranslationUnit& tu) {
    // renames variables with fresh names to avoid name collisions
    struct AnonymiseVars : AstNodeMapper {
        AnonymiseVars(AstTranslationUnit& tu) : tu(tu) {}
        AstTranslationUnit& tu;
        mutable std::map<std::string, std::string> rename;

        Own<AstNode> operator()(Own<AstNode> node) const {
            if (auto var = as<AstVariable>(node)) {
                auto& fresh = rename[var->getName()];
                if (fresh.empty()) fresh = tu.freshName();

                auto cpy = clone(var);
                cpy->setName(fresh);
                return cpy;
            }

            node->apply(*this);
            return node;
        }
    };

    struct Inliner : AstNodeMapper {
        Inliner(AstTranslationUnit& tu) : tu(tu) {}

        AstTranslationUnit& tu;
        RelationDetailCache& relCache = *tu.getAnalysis<RelationDetailCache>();
        mutable bool appliedInlining = false;

        Own<AstNode> operator()(Own<AstNode> node) const {
            auto atom = as<AstAtom>(node);
            auto rel = atom ? relCache.getRelation(atom->getQualifiedName()) : nullptr;
            if (rel && rel->hasQualifier(RelationQualifier::INLINE)) {
                appliedInlining = true;

                // well that was underwhelming: the inline relation had no clauses.
                auto clauses = relCache.getClauses(rel->getQualifiedName());
                if (clauses.empty()) {
                    auto boring = mk<AstBooleanConstraint>(false);
                    boring->setSrcLoc(atom->getSrcLoc());
                    return boring;
                }

                auto&& callArgs = atom->getArguments();
                auto replacement = mk<AstBody>();
                for (auto&& clause : clauses) {
                    auto clauseAnon = clone(clause);
                    clauseAnon->apply(AnonymiseVars(tu));
                    auto&& headExprs = clauseAnon->getHead()->getArguments();
                    assert(callArgs.size() == headExprs.size());

                    // FIXME: If the AST defs were proper PODs we could avoid a lot of pointless cloning.
                    //        There's no point in having getters if you want a POD.
                    AstBody::Conjunction newTerms;
                    // copy the body (if any)
                    if (!clauseAnon->getBody().disjunction.empty())
                        newTerms.push_back(clone(&clauseAnon->getBody()));

                    // add constraints to bind our args to the clause head
                    size_t i = 0;
                    for (auto&& arg : callArgs) {
                        newTerms.push_back(mk<AstBinaryConstraint>(
                                BinaryConstraintOp::EQ, clone(arg), clone(headExprs.at(i++))));
                    }

                    // EDGE CASE: inlining `a().` results in an empty `newTerms`.
                    //            Empty conjunctions aren't legal; fix by adding a `true`.
                    if (newTerms.empty()) newTerms.push_back(mk<AstBooleanConstraint>(true));

                    replacement->disjunction.push_back(std::move(newTerms));
                }

                node = std::move(replacement);
            }

            // full recursion required b/c atoms can appear inside arguments via aggregates.
            node->apply(*this);  // depth first post order
            return node;
        }

        bool operator()(AstClause& clause) {
            appliedInlining = false;
            clause.getBody().apply(*this);
            clause.getHead()->apply(*this);  // also check for head aggregate body inlines
            return appliedInlining;
        }
    } inliner(tu);

    // Keep trying to inline things until we reach a fixed point.
    // Since we know there are no cyclic dependencies between inlined relations, this will necessarily
    // terminate.
    bool changed = false;
    AstProgram& program = *tu.getProgram();
    RelationDetailCache& relCache = *tu.getAnalysis<RelationDetailCache>();
    for (AstRelation* rel : program.getRelations()) {
        // Skip if the relation is going to be inlined
        if (rel->hasQualifier(RelationQualifier::INLINE)) continue;

        // Go through the relation's clauses and try inlining them
        for (auto* clause : relCache.getClauses(rel->getQualifiedName())) changed |= inliner(*clause);
    }

    return changed;
}

}  // namespace souffle
