/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstFunctorDeclaration.h
 *
 * Defines external functors.
 *
 ***********************************************************************/

#pragma once

#include "AstNode.h"
#include "RamTypes.h"
#include "Util.h"
#include <algorithm>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

/**
 * AstFunctorDeclaration
 */

class AstFunctorDeclaration : public AstNode {
public:
    AstFunctorDeclaration(
            const std::string& name, std::vector<TypeAttribute> argsTypes, TypeAttribute returnType)
            : name(name), argsTypes(std::move(argsTypes)), returnType(returnType) {
        assert(name.length() > 0 && "functor name is empty");
    }

    /** get name */
    const std::string& getName() const {
        return name;
    }

    /** get type */
    const std::vector<TypeAttribute>& getArgsTypes() const {
        return argsTypes;
    }

    TypeAttribute getReturnType() const {
        return returnType;
    }

    /** get number of arguments */
    size_t getArity() const {
        return argsTypes.size();
    }

    /** clone */
    AstFunctorDeclaration* clone() const override {
        auto* res = new AstFunctorDeclaration(name, argsTypes, returnType);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    void print(std::ostream& out) const override {
        auto convert = [&](TypeAttribute type) {
            switch (type) {
                case TypeAttribute::Signed:
                    return "number";
                case TypeAttribute::Symbol:
                    return "symbol";
                case TypeAttribute::Float:
                    return "float";
                case TypeAttribute::Unsigned:
                    return "unsigned";
                case TypeAttribute::Record:
                    fatal("unhandled `TypeAttribute`");
            }

            UNREACHABLE_BAD_CASE_ANALYSIS
        };

        format(out, ".declfun %s(%s): %s\n", name, join(map(argsTypes, convert), ","), convert(returnType));
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstFunctorDeclaration&>(node);
        return name == other.name && argsTypes == other.argsTypes && returnType == other.returnType;
    }

    /** name of functor */
    const std::string name;

    /** Types of arguments */
    const std::vector<TypeAttribute> argsTypes;

    /** Type of the return value */
    const TypeAttribute returnType;
};

}  // end of namespace souffle
