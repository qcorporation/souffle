/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstLiteral.h
 *
 * Define classes for Literals and its subclasses atoms, negated atoms,
 * and binary relations.
 *
 ***********************************************************************/

#pragma once

#include "AstAbstract.h"
#include "AstNode.h"
#include "AstQualifiedName.h"
#include "BinaryConstraintOps.h"
#include "Util.h"

#include <iostream>
#include <list>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "AstAbstract.h"
#include "AstNode.h"

namespace souffle {

class AstRelation;
class AstClause;
class AstProgram;
class AstAtom;

class AstBody : public AstLiteral {
public:
    using Conjunction = std::vector<std::unique_ptr<AstLiteral>>;
    using Disjunction = std::vector<Conjunction>;

    AstBody() = default;
    AstBody(Disjunction dnf) : dnf(std::move(dnf)) {}
    AstBody(std::unique_ptr<AstLiteral> literal) {
        Conjunction res;
        res.emplace_back(std::move(literal));
        dnf.emplace_back(std::move(res));
    }

    std::vector<AstClause*> toClauseBodies() const;

    void normalize();

    void conjunct(std::unique_ptr<AstLiteral> literal);

    void disjunct(AstBody&& other);

    AstBody* clone() const override;

    void apply(const AstNodeMapper& map) override;

    std::vector<const AstNode*> getChildNodes() const override;

    // FIXME: helper for now
    std::vector<const AstLiteral*> getChildLiterals() const;

    // -- factory functions --

    // void negate();

    // static RuleBody getTrue();

    // static RuleBody getFalse();

    // static RuleBody atom(AstAtom* atom);

    // static RuleBody constraint(AstConstraint* constraint);

    // friend std::ostream& operator<<(std::ostream& out, const RuleBody& body);

protected:
    void print(std::ostream& os) const override;

    bool equal(const AstNode& node) const override;

private:
    Disjunction dnf;
};

/**
 * Subclass of Literal that represents the use of a relation
 * either in the head or in the body of a Clause, e.g., parent(x,y).
 * The arguments of the atom can be variables or constants.
 */
class AstAtom : public AstLiteral {
public:
    AstAtom(AstQualifiedName name = AstQualifiedName()) : name(std::move(name)) {}

    AstAtom(AstQualifiedName name, std::vector<std::unique_ptr<AstArgument>> args, SrcLocation srcLoc)
            : name(std::move(name)), arguments(std::move(args)) {
        setSrcLoc(srcLoc);
    }

    /** get qualified name */
    const AstQualifiedName& getQualifiedName() const {
        return name;
    }

    /** get arity of the atom */
    size_t getArity() const {
        return arguments.size();
    }

    /** set qualified name */
    void setQualifiedName(const AstQualifiedName& n) {
        name = n;
    }

    /** add argument to the atom */
    void addArgument(std::unique_ptr<AstArgument> arg) {
        arguments.push_back(std::move(arg));
    }

    /** get arguments */
    std::vector<AstArgument*> getArguments() const {
        return toPtrVector(arguments);
    }

    AstAtom* clone() const override {
        auto res = new AstAtom(name);
        res->setSrcLoc(getSrcLoc());
        for (const auto& cur : arguments) {
            res->arguments.emplace_back(cur->clone());
        }
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& arg : arguments) {
            arg = map(std::move(arg));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> res;
        for (auto& cur : arguments) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    void print(std::ostream& os) const override {
        os << getQualifiedName() << "(";
        os << join(arguments, ",", print_deref<std::unique_ptr<AstArgument>>());
        os << ")";
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstAtom&>(node);
        return name == other.name && equal_targets(arguments, other.arguments);
    }

    /** name */
    AstQualifiedName name;

    /** arguments */
    std::vector<std::unique_ptr<AstArgument>> arguments;
};

/**
 * Subclass of Literal that represents a negated literal, * e.g., !parent(x,y).
 */
class AstNegation : public AstLiteral {
public:
    AstNegation(std::unique_ptr<AstLiteral> literal) : literal(std::move(literal)) {}

    /** get negated lit */
    AstLiteral* getLiteral() const {
        return literal.get();
    }

    AstAtom* getAtom() const {
        return dynamic_cast<AstAtom*>(literal.get());
    }

    AstNegation* clone() const override {
        auto* res = new AstNegation(souffle::clone(literal));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        literal = map(std::move(literal));
    }

    std::vector<const AstNode*> getChildNodes() const override {
        return {literal.get()};
    }

protected:
    void print(std::ostream& os) const override {
        if (dynamic_cast<AstBody*>(literal.get())) {
            os << "!(" << *literal << ")";
        } else {
            os << "!" << *literal;
        }
    }

    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstNegation*>(&node));
        const auto& other = static_cast<const AstNegation&>(node);
        return equal_ptr(literal, other.literal);
    }

    /** negated literal */
    std::unique_ptr<AstLiteral> literal;
};

/**
 * Subclass of Literal that represents a negated atom, * e.g., !parent(x,y).
 * A Negated atom occurs in a body of clause and cannot occur in a head of a clause.
 *
 * Specialised for provenance: used for existence check that tuple doesn't already exist
 */
class AstProvenanceNegation : public AstNegation {
public:
    AstProvenanceNegation(std::unique_ptr<AstLiteral> literal) : AstNegation(std::move(literal)) {}

    AstProvenanceNegation* clone() const override {
        auto* res = new AstProvenanceNegation(souffle::clone(literal));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    void print(std::ostream& os) const override {
        os << "prov!" << *literal;
    }
};

/**
 * Boolean Constraint
 *
 * Representing either 'true' or 'false' values
 */
class AstBooleanConstraint : public AstConstraint {
public:
    AstBooleanConstraint(bool truthValue) : truthValue(truthValue) {}

    /** check whether constraint holds */
    bool isTrue() const {
        return truthValue;
    }

    /** set truth value */
    void set(bool value) {
        truthValue = value;
    }

    AstBooleanConstraint* clone() const override {
        auto* res = new AstBooleanConstraint(truthValue);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    void print(std::ostream& os) const override {
        os << (truthValue ? "true" : "false");
    }

    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstBooleanConstraint*>(&node));
        const auto& other = static_cast<const AstBooleanConstraint&>(node);
        return truthValue == other.truthValue;
    }

    /** truth value */
    bool truthValue;
};

/**
 * Subclass of Constraint that represents a binary constraint
 * e.g., x = y.
 */
class AstBinaryConstraint : public AstConstraint {
public:
    AstBinaryConstraint(
            BinaryConstraintOp o, std::unique_ptr<AstArgument> ls, std::unique_ptr<AstArgument> rs)
            : operation(o), lhs(std::move(ls)), rhs(std::move(rs)) {}

    /** get LHS argument */
    AstArgument* getLHS() const {
        return lhs.get();
    }

    /** get RHS argument */
    AstArgument* getRHS() const {
        return rhs.get();
    }

    /** get binary operator */
    BinaryConstraintOp getOperator() const {
        return operation;
    }

    /** set binary operator */
    void setOperator(BinaryConstraintOp op) {
        operation = op;
    }

    AstBinaryConstraint* clone() const override {
        auto* res = new AstBinaryConstraint(operation, std::unique_ptr<AstArgument>(lhs->clone()),
                std::unique_ptr<AstArgument>(rhs->clone()));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        lhs = map(std::move(lhs));
        rhs = map(std::move(rhs));
    }

    std::vector<const AstNode*> getChildNodes() const override {
        return {lhs.get(), rhs.get()};
    }

protected:
    void print(std::ostream& os) const override {
        os << *lhs;
        os << " " << toBinaryConstraintSymbol(operation) << " ";
        os << *rhs;
    }

    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstBinaryConstraint*>(&node));
        const auto& other = static_cast<const AstBinaryConstraint&>(node);
        return operation == other.operation && equal_ptr(lhs, other.lhs) && equal_ptr(rhs, other.rhs);
    }

    /** constraint operator */
    BinaryConstraintOp operation;

    /** left-hand side of binary constraint */
    std::unique_ptr<AstArgument> lhs;

    /** right-hand side of binary constraint */
    std::unique_ptr<AstArgument> rhs;
};

/**
 * FIXME:
 */
class AstDisjunction : public AstLiteral {
public:
    AstDisjunction(std::vector<std::unique_ptr<AstLiteral>> literals) : literals(std::move(literals)) {}

    std::vector<AstLiteral*> getLiterals() const {
        return toPtrVector(literals);
    }

    AstDisjunction* clone() const override {
        auto* res = new AstDisjunction(souffle::clone(literals));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto&& x : literals) {
            x = map(std::move(x));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> result;
        for (auto&& x : literals) {
            result.push_back(x.get());
        }
        return result;
    }

protected:
    void print(std::ostream& os) const override {
        os << join(literals, ", ", [](auto& os, auto&& x) {
            if (dynamic_cast<const AstDisjunction*>(x.get()))
                os << "(" << *x << ")";
            else
                os << *x;
        });
    }

    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstDisjunction*>(&node));
        const auto& other = static_cast<const AstDisjunction&>(node);
        return equal_targets(literals, other.literals);
    }

    /** FIXME literal */
    std::vector<std::unique_ptr<AstLiteral>> literals;
};

/**
 * FIXME
 */
class AstConjunction : public AstLiteral {
public:
    AstConjunction(std::vector<std::unique_ptr<AstLiteral>> literals) : literals(std::move(literals)) {}

    std::vector<AstLiteral*> getLiterals() const {
        return toPtrVector(literals);
    }

    AstConjunction* clone() const override {
        auto* res = new AstConjunction(souffle::clone(literals));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto&& x : literals) {
            x = map(std::move(x));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> result;
        for (auto&& x : literals) {
            result.push_back(x.get());
        }
        return result;
    }

protected:
    void print(std::ostream& os) const override {
        os << join(literals, ", ", [](auto& os, auto&& x) {
            if (dynamic_cast<const AstConjunction*>(x.get()))
                os << "(" << *x << ")";
            else
                os << *x;
        });
    }

    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstConjunction*>(&node));
        const auto& other = static_cast<const AstConjunction&>(node);
        return equal_targets(literals, other.literals);
    }

    /** negated literal */
    std::vector<std::unique_ptr<AstLiteral>> literals;
};

}  // end of namespace souffle
