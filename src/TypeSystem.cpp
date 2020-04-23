/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TypeSystem.cpp
 *
 * Covers basic operations constituting Souffle's type system.
 *
 ***********************************************************************/

#include "TypeSystem.h"
#include "RamTypes.h"
#include "Util.h"
#include <cassert>

namespace souffle {

void SubsetType::print(std::ostream& out) const {
    out << getName() << " <: " << baseType;
}

void UnionType::add(const Type& type) {
    assert(environment.isType(type));
    elementTypes.push_back(&type);
}

void UnionType::print(std::ostream& out) const {
    out << getName() << " = "
        << join(elementTypes, " | ", [](std::ostream& out, const Type* type) { out << type->getName(); });
}

void RecordType::add(const std::string& name, const Type& type) {
    assert(environment.isType(type));
    fields.push_back(Field({name, type}));
}

void RecordType::print(std::ostream& out) const {
    out << getName() << " = ";
    if (fields.empty()) {
        out << "()";
        return;
    }
    out << "( " << join(fields, " , ", [](std::ostream& out, const RecordType::Field& f) {
        out << f.name << " : " << f.type.getName();
    }) << " )";
}

TypeSet TypeEnvironment::initializeConstantTypes() {
    auto& signedConstant = createType<ConstantType>("numberConstant");
    auto& floatConstant = createType<ConstantType>("floatConstant");
    auto& symbolConstant = createType<ConstantType>("symbolConstant");
    auto& unsignedConstant = createType<ConstantType>("unsignedConstant");

    return TypeSet(signedConstant, floatConstant, symbolConstant, unsignedConstant);
}

TypeSet TypeEnvironment::initializePrimitiveTypes() {
#define CREATE_PRIMITIVE(TYPE) \
    auto& TYPE##Type =         \
            createType<PrimitiveType>(#TYPE, static_cast<const ConstantType&>(getType(#TYPE "Constant")));

    CREATE_PRIMITIVE(number);
    CREATE_PRIMITIVE(float);
    CREATE_PRIMITIVE(symbol);
    CREATE_PRIMITIVE(unsigned);

    return TypeSet(numberType, floatType, symbolType, unsignedType);

#undef CREATE_PRIMITIVE
}

bool TypeEnvironment::isType(const AstQualifiedName& ident) const {
    return types.find(ident) != types.end();
}

bool TypeEnvironment::isType(const Type& type) const {
    return this == &type.getTypeEnvironment();
}

const Type& TypeEnvironment::getType(const AstQualifiedName& ident) const {
    return *types.at(ident);
}
Type& TypeEnvironment::getType(const AstQualifiedName& ident) {
    return *types.at(ident);
}

namespace {

/**
 * A visitor for Types.
 */
template <typename R>
struct TypeVisitor {
    virtual ~TypeVisitor() = default;

    R operator()(const Type& type) const {
        return visit(type);
    }

#define FORWARD(TYPE) \
    if (auto* t = dynamic_cast<const TYPE##Type*>(&type)) return visit##TYPE##Type(*t);

    virtual R visit(const Type& type) const {
        FORWARD(Constant);
        FORWARD(Subset);
        FORWARD(Union);
        FORWARD(Record);

        fatal("Unsupported type encountered!");
    }
#undef FORWARD

#define VISIT(TYPE)                                             \
    virtual R visit##TYPE##Type(const TYPE##Type& type) const { \
        return visitType(type);                                 \
    }

    VISIT(Constant)
    VISIT(Subset)
    VISIT(Union)
    VISIT(Record)

    virtual R visitType(const Type& /*type*/) const {
        return R();
    }

#undef VISIT
};

/**
 * A visitor for types visiting each type only once (effectively breaking
 * recursive cycles).
 */
template <typename R>
class VisitOnceTypeVisitor : public TypeVisitor<R> {
protected:
    mutable std::map<const Type*, R> seen;

public:
    R visit(const Type& type) const override {
        auto pos = seen.find(&type);
        if (pos != seen.end()) {
            return pos->second;
        }
        auto& res = seen[&type];  // mark as seen
        return res = TypeVisitor<R>::visit(type);
    }
};

/**
 * Determines whether the given type is a sub-type of the given root type.
 */
bool isOfRootType(const Type& type, const Type& root) {
    struct visitor : public VisitOnceTypeVisitor<bool> {
        const Type& root;

        explicit visitor(const Type& root) : root(root) {}

        bool visitConstantType(const ConstantType& type) const override {
            return type == root;
        }
        bool visitSubsetType(const SubsetType& type) const override {
            return type == root || isOfRootType(type.getBaseType(), root);
        }
        bool visitUnionType(const UnionType& type) const override {
            return type == root ||
                   all_of(type.getElementTypes(), [&](const Type* cur) { return this->visit(*cur); });
        }

        bool visitRecordType(const RecordType& type) const override {
            return type == root;
        }

        bool visitType(const Type& /*unused*/) const override {
            return false;
        }
    };

    return visitor(root).visit(type);
}

}  // namespace

/* generate unique type qualifier string for a type */
std::string getTypeQualifier(const Type& type) {
    struct visitor : public VisitOnceTypeVisitor<std::string> {
        std::string visitUnionType(const UnionType& type) const override {
            std::string str = visitType(type);
            str += "[";
            bool first = true;
            for (auto unionType : type.getElementTypes()) {
                if (first) {
                    first = false;
                } else {
                    str += ",";
                }
                str += visit(*unionType);
            }
            str += "]";
            return str;
        }

        std::string visitRecordType(const RecordType& type) const override {
            std::string str = visitType(type);
            str += "{";
            bool first = true;
            for (auto field : type.getFields()) {
                if (first) {
                    first = false;
                } else {
                    str += ",";
                }
                str += field.name;
                str += "#";
                str += visit(field.type);
            }
            str += "}";
            return str;
        }

        std::string visitType(const Type& type) const override {
            std::string str;

            switch (getTypeAttribute(type)) {
                case TypeAttribute::Signed:
                    str.append("i");
                    break;
                case TypeAttribute::Unsigned:
                    str.append("u");
                    break;
                case TypeAttribute::Float:
                    str.append("f");
                    break;
                case TypeAttribute::Symbol:
                    str.append("s");
                    break;
                case TypeAttribute::Record:
                    str.append("r");
                    break;
            }
            str.append(":");
            str.append(toString(type.getName()));
            seen[&type] = str;
            return str;
        }
    };

    return visitor().visit(type);
}

bool hasSignedType(const TypeSet& types) {
    return types.isAll() || any_of(types, (bool (*)(const Type&)) & isNumberType);
}

bool hasUnsignedType(const TypeSet& types) {
    return types.isAll() || any_of(types, (bool (*)(const Type&)) & isUnsignedType);
}

bool hasFloatType(const TypeSet& types) {
    return types.isAll() || any_of(types, (bool (*)(const Type&)) & isFloatType);
}

bool isFloatType(const Type& type) {
    return isOfRootType(type, type.getTypeEnvironment().getConstantType(TypeAttribute::Float));
}

bool isFloatType(const TypeSet& s) {
    return !s.empty() && !s.isAll() && all_of(s, (bool (*)(const Type&)) & isFloatType);
}

bool isNumberType(const Type& type) {
    return isOfRootType(type, type.getTypeEnvironment().getConstantType(TypeAttribute::Signed));
}

bool isNumberType(const TypeSet& s) {
    return !s.empty() && !s.isAll() && all_of(s, (bool (*)(const Type&)) & isNumberType);
}

bool isUnsignedType(const Type& type) {
    return isOfRootType(type, type.getTypeEnvironment().getConstantType(TypeAttribute::Unsigned));
}

bool isUnsignedType(const TypeSet& s) {
    return !s.empty() && !s.isAll() && all_of(s, (bool (*)(const Type&)) & isUnsignedType);
}

bool isSymbolType(const Type& type) {
    return isOfRootType(type, type.getTypeEnvironment().getConstantType(TypeAttribute::Symbol));
}

bool isSymbolType(const TypeSet& s) {
    return !s.empty() && !s.isAll() && all_of(s, (bool (*)(const Type&)) & isSymbolType);
}

bool isRecordType(const Type& type) {
    return isA<RecordType>(type);
}

bool isRecordType(const TypeSet& s) {
    return !s.empty() && !s.isAll() && all_of(s, (bool (*)(const Type&)) & isA<RecordType>);
}

bool isSubtypeOf(const Type& a, const Type& b) {
    assert(&a.getTypeEnvironment() == &b.getTypeEnvironment() &&
            "Types must be in the same type environment");

    if (isOfRootType(a, b)) {
        return true;
    }

    if (isA<UnionType>(a)) {
        return all_of(static_cast<const UnionType&>(a).getElementTypes(),
                [&b](const Type* type) { return isSubtypeOf(*type, b); });
    }

    if (isA<UnionType>(b)) {
        return any_of(static_cast<const UnionType&>(b).getElementTypes(),
                [&a](const Type* type) { return isSubtypeOf(a, *type); });
    }

    return false;
}

void TypeEnvironment::print(std::ostream& out) const {
    out << "Types:\n";
    for (const auto& cur : types) {
        out << "\t" << *cur.second << "\n";
    }
}

TypeSet getGreatestCommonSubtypes(const Type& a, const Type& b) {
    assert(&a.getTypeEnvironment() == &b.getTypeEnvironment() &&
            "Types must be in the same type environment");

    if (isSubtypeOf(a, b)) {
        return TypeSet(a);
    }
    if (isSubtypeOf(b, a)) {
        return TypeSet(b);
    }

    // last option: if both are unions with common sub-types
    TypeSet res;
    if (isA<UnionType>(a) && isA<UnionType>(b)) {
        // collect common sub-types of union types
        struct collector : public TypeVisitor<void> {
            const Type& b;
            TypeSet& res;
            collector(const Type& b, TypeSet& res) : b(b), res(res) {}

            void visit(const Type& type) const override {
                if (isSubtypeOf(type, b)) {
                    res.insert(type);
                } else {
                    TypeVisitor<void>::visit(type);
                }
            }
            void visitUnionType(const UnionType& type) const override {
                for (const auto& cur : type.getElementTypes()) {
                    visit(*cur);
                }
            }
        };

        // collect all common sub-types
        collector(b, res).visit(a);
    }

    // otherwise there is no common super type
    return res;
}

TypeSet getGreatestCommonSubtypes(const TypeSet& set) {
    // Edge cases.
    if (set.empty() || set.isAll()) {
        return TypeSet();
    }

    TypeSet greatestCommonSubtypes;
    greatestCommonSubtypes.insert(*set.begin());

    for (auto& type : set) {
        greatestCommonSubtypes = getGreatestCommonSubtypes(TypeSet(type), greatestCommonSubtypes);
    }

    return greatestCommonSubtypes;
}

TypeSet getGreatestCommonSubtypes(const TypeSet& a, const TypeSet& b) {
    // special cases
    if (a.empty()) {
        return a;
    }
    if (b.empty()) {
        return b;
    }

    if (a.isAll()) {
        return b;
    }
    if (b.isAll()) {
        return a;
    }

    // compute pairwise greatest common sub types
    TypeSet res;
    for (const Type& x : a) {
        for (const Type& y : b) {
            res.insert(getGreatestCommonSubtypes(x, y));
        }
    }
    return res;
}

}  // end of namespace souffle
