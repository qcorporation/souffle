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
namespace {

using souffle::AstBody;
using souffle::AstLiteral;

static bool literalEquals(const AstLiteral* a, const AstLiteral* b) {
    return *a == *b;
}

static bool conjunctionEquals(AstBody::Conjunction&& a, AstBody::Conjunction&& b) {
    if (a.size() != b.size()) {
        return false;
    }
    for (auto&& i : a) {
        for (auto&& j : b) {
            if (literalEquals(i.get(), j.get())) {
                return true;
            }
        }
        return false;
    }
    return true;
}

static bool isSubsetOf(const AstBody::Conjunction& a, const AstBody::Conjunction& b) {
    if (a.size() > b.size()) {
        return false;
    }
    for (auto&& i : a) {
        for (auto&& j : b) {
            if (literalEquals(i.get(), j.get())) {
                return true;
            }
        }
        return false;
    }
    return true;
}

static void insert(AstBody::Conjunction& cl, std::unique_ptr<AstLiteral> literal) {
    for (auto&& cur : cl) {
        if (literalEquals(cur.get(), literal.get())) {
            return;
        }
    }
    cl.emplace_back(std::move(literal));
}

static void insertConjunction(AstBody::Disjunction& dnf, AstBody::Conjunction&& cls) {
    for (auto&& cnf : dnf) {
        if (isSubsetOf(cnf, cls)) {
            return;
        }
    }
    AstBody::Disjunction res;
    for (auto&& cur : dnf) {
        if (!isSubsetOf(cls, cur)) {
            res.push_back(std::move(cur));
        }
    }
    res.swap(dnf);
    dnf.push_back(std::move(cls));
}

}  // namespace

void AstBody::normalize() {
    // TODO
}

AstBody* AstBody::clone() const {
    auto* res = new AstBody(souffle::clone(dnf));
    res->setSrcLoc(getSrcLoc());
    return res;
}

void AstBody::apply(const AstNodeMapper& map) {
    for (auto&& conj : dnf) {
        for (auto&& lit : conj) {
            lit = map(std::move(lit));
        }
    }
}

std::vector<const AstNode*> AstBody::getChildNodes() const {
    std::vector<const AstNode*> result;
    for (auto&& conj : dnf) {
        for (auto&& lit : conj) {
            result.push_back(lit.get());
        }
    }
    return result;
}

// FIXME: helper for now
std::vector<const AstLiteral*> AstBody::getChildLiterals() const {
    std::vector<const AstLiteral*> result;
    for (auto&& conj : dnf) {
        for (auto&& lit : conj) {
            result.push_back(lit.get());
        }
    }
    return result;
}

void AstBody::print(std::ostream& os) const {
    os << join(dnf, ";",
            [](auto& os, auto&& conj) { os << join(conj, ",", print_deref<std::unique_ptr<AstLiteral>>()); });
}

std::vector<AstClause*> AstBody::toClauseBodies() const {
    // collect clause results
    std::vector<AstClause*> bodies;
    for (auto&& cnf : dnf) {
        bodies.push_back(new AstClause());
        AstClause& clause = *bodies.back();

        for (auto&& literal : cnf) {
            // extract literal
            AstLiteral* base = literal->clone();

            // FIXME: we should have a ASTNegation definition already, not sure why we need this
            // if (lit.negated) {
            //     // negate
            //     if (auto* atom = dynamic_cast<AstAtom*>(base)) {
            //         base = new AstNegation(std::unique_ptr<AstAtom>(atom));
            //         base->setSrcLoc(atom->getSrcLoc());
            //     } else if (auto* cstr = dynamic_cast<AstConstraint*>(base)) {
            //         negateConstraint(cstr);
            //     }
            // }

            // add to result
            clause.addToBody(std::unique_ptr<AstLiteral>(base));
        }
    }

    return bodies;
}

bool AstBody::equal(const AstNode& node) const {

    assert(nullptr != dynamic_cast<const AstBody*>(&node));
    const auto& other = static_cast<const AstBody&>(node);
    if( dnf.size() != other.dnf.size() ){
    	return false;
    }

    // FIXME: implement properly
    return true;
}

// void RuleBody::negate() {
//     std::cerr << "RuleBody::negate" << std::endl;
//     RuleBody res = getTrue();

//     for (const clause& cur : dnf) {
//         RuleBody step = getFalse();

//         for (const literal& lit : cur) {
//             step.dnf.push_back(clause());
//             clause& cl = step.dnf.back();
//             cl.emplace_back(literal{!lit.negated, std::unique_ptr<AstLiteral>(lit.atom->clone())});
//         }

//         res.conjunct(std::move(step));
//     }

//     dnf.swap(res.dnf);
// }

void AstBody::conjunct(std::unique_ptr<AstLiteral> literal) {
    Disjunction res;
    AstBody other(std::move(literal));

    for (auto&& clauseA : dnf) {
        for (auto&& clauseB : other.dnf) {
            // create a new clause in result
            Conjunction cur;

            // it is the concatenation of the two clauses
            for (auto&& literal : clauseA) {
                cur.emplace_back(literal->clone());
            }
            for (auto&& literal : clauseB) {
                insert(cur, std::move(literal));
            }
            insertConjunction(res, std::move(cur));
        }
    }

    // update local dnf
    dnf.swap(res);
}

void AstBody::disjunct(AstBody&& other) {
    for (auto&& cur : other.dnf) {
        insertConjunction(dnf, std::move(cur));
    }
}

// // -- factory functions --

// RuleBody RuleBody::getTrue() {
//     std::cerr << "RuleBody::getTrue" << std::endl;
//     RuleBody body;
//     body.dnf.push_back(clause());
//     return body;
// }

// RuleBody RuleBody::getFalse() {
//     std::cerr << "RuleBody::getFalse" << std::endl;
//     return RuleBody();
// }

// RuleBody RuleBody::atom(AstAtom* atom) {
//     std::cerr << "RuleBody::atom" << std::endl;
//     RuleBody body;
//     body.dnf.push_back(clause());
//     auto& clause = body.dnf.back();
//     clause.emplace_back(false, std::unique_ptr<AstAtom>(atom));
//     return body;
// }

// RuleBody RuleBody::constraint(AstConstraint* constraint) {
//     std::cerr << "RuleBody::constraint" << std::endl;
//     RuleBody body;
//     body.dnf.push_back(clause());
//     auto& clause = body.dnf.back();
//     clause.emplace_back(false, std::unique_ptr<AstLiteral>(constraint));
//     return body;
// }

// std::ostream& operator<<(std::ostream& out, const RuleBody& body) {
//     std::cerr << "RuleBody::operator<<" << std::endl;
//     return out << join(body.dnf, ";", [](std::ostream& out, const RuleBody::clause& cur) {
//         out << join(cur, ",", [](std::ostream& out, const RuleBody::literal& l) {
//             if (l.negated) {
//                 out << "!";
//             }
//             out << *l.atom;
//         });
//     });
// }

}  // namespace souffle
