/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstRelation.h
 *
 * Defines class Relation that represents relations in a Datalog program.
 * A relation can either be an IDB or EDB relation.
 *
 ***********************************************************************/

#pragma once

#include "AstAttribute.h"
#include "AstClause.h"
#include "AstIO.h"
#include "AstNode.h"
#include "AstQualifiedName.h"
#include "AstType.h"
#include "Global.h"
#include "RelationTag.h"

#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <cctype>

namespace souffle {

/*!
 * @class Relation
 * @brief Intermediate representation of a datalog relation
 *
 * A relation has a name, types of its arguments, qualifier type,
 * and dependencies to other relations.
 *
 */
class AstRelation : public AstNode {
public:
    AstRelation(AstQualifiedName name = {}, std::vector<std::unique_ptr<AstAttribute>> attrs = {})
            : name(std::move(name)), attributes(std::move(attrs)){};

    /** get qualified relation name */
    const AstQualifiedName& getQualifiedName() const {
        return name;
    }

    /** Set name for this relation */
    void setQualifiedName(const AstQualifiedName& n) {
        name = n;
    }

    /** Add a new used type to this relation */
    void addAttribute(std::unique_ptr<AstAttribute> attr) {
        assert(attr && "Undefined attribute");
        attributes.push_back(std::move(attr));
    }

    /** Return the arity of this relation */
    size_t getArity() const {
        return attributes.size();
    }

    /** Get relation attributes */
    std::vector<AstAttribute*> getAttributes() const {
        return toPtrVector(attributes);
    }

    /** Get relation qualifiers */
    const std::set<RelationQualifier>& getQualifiers() const {
        return qualifiers;
    }

    /** Add qualifier to this relation */
    void addQualifier(RelationQualifier q) {
        qualifiers.insert(q);
    }

    /** Remove qualifier from this relation */
    void removeQualifier(RelationQualifier q) {
        qualifiers.erase(q);
    }

    /** Get relation representation */
    RelationRepresentation getRepresentation() const {
        return representation;
    }

    /** Set relation representation */
    void setRepresentation(RelationRepresentation representation) {
        this->representation = representation;
    }

    /** check for a relation qualifier */
    bool hasQualifier(RelationQualifier q) const {
        return qualifiers.find(q) != qualifiers.end();
    }

    AstRelation* clone() const override {
        auto res = new AstRelation();
        res->name = name;
        res->setSrcLoc(getSrcLoc());
        for (const auto& cur : attributes) {
            res->attributes.emplace_back(cur->clone());
        }
        for (auto cur : qualifiers) {
            res->qualifiers.insert(cur);
        }
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& cur : attributes) {
            cur = map(std::move(cur));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> res;
        for (const auto& cur : attributes) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    void print(std::ostream& os) const override {
        os << ".decl " << this->getQualifiedName() << "(";
        if (!attributes.empty()) {
            os << attributes[0]->getAttributeName() << ":" << attributes[0]->getTypeName();

            for (size_t i = 1; i < attributes.size(); ++i) {
                os << "," << attributes[i]->getAttributeName() << ":" << attributes[i]->getTypeName();
            }
        }
        os << ") ";

        os << join(qualifiers, " ") << " ";
        os << representation;
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstRelation&>(node);
        return name == other.name && equal_targets(attributes, other.attributes);
    }

    /** Name of relation */
    AstQualifiedName name;

    /** Attributes of the relation */
    std::vector<std::unique_ptr<AstAttribute>> attributes;

    /** Qualifiers of relation */
    std::set<RelationQualifier> qualifiers;

    /** Datastructure to use for this relation */
    RelationRepresentation representation{RelationRepresentation::DEFAULT};
};

/**
 * Lexicographical order for AstRelation
 * using the qualified name as an ordering criteria.
 */
struct AstNameComparison {
    bool operator()(const AstRelation* x, const AstRelation* y) const {
        if (x != nullptr && y != nullptr) {
            return x->getQualifiedName() < y->getQualifiedName();
        }
        return y != nullptr;
    }
};

using AstRelationSet = std::set<const AstRelation*, AstNameComparison>;

}  // end of namespace souffle
