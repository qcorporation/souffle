/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstBody.cpp
 *
 * Defines class AstBody to represents rule bodies.
 *
 ***********************************************************************/

#include "AstClause.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstUtils.h"
#include "Util.h"
#include <memory>
#include <ostream>
#include <utility>
#include <vector>

namespace souffle {

void AstBody::canonicalise() {
    disjunction = filter(std::move(disjunction), [](auto&& conj) { return !conj.empty(); });
}

AstBody* AstBody::clone() const {
    auto* res = new AstBody(souffle::clone(disjunction));
    res->setSrcLoc(getSrcLoc());
    return res;
}

void AstBody::apply(const AstNodeMapper& map) {
    for (auto&& conj : disjunction) {
        for (auto&& lit : conj) {
            lit = map(std::move(lit));
        }
    }
}

std::vector<const AstNode*> AstBody::getChildNodes() const {
    std::vector<const AstNode*> result;
    for (auto&& conj : disjunction) {
        for (auto&& lit : conj) {
            result.push_back(lit.get());
        }
    }
    return result;
}

void AstBody::print(std::ostream& os) const {
    os << join(disjunction, "; ", [](auto& os, auto&& conj) {
        os << join(conj, ", ", print_deref<std::unique_ptr<AstLiteral>>());
    });
}

bool AstBody::equal(const AstNode& node) const {
    auto& other = dynamic_cast<const AstBody&>(node);
    return equal_targets(disjunction, other.disjunction);
}

}  // namespace souffle
