/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TypeSystem.h
 *
 * Covers basic operations constituting Souffle's type system.
 *
 ***********************************************************************/

#pragma once

#include "AstType.h"
#include "IterUtils.h"
#include "Util.h"
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

// forward declaration
class TypeEnvironment;

/**
 * An abstract base class for types to be covered within a type environment.
 */
class Type {
public:
    Type(const Type& other) = delete;

    virtual ~Type() = default;

    const AstQualifiedName& getName() const {
        return name;
    }

    const TypeEnvironment& getTypeEnvironment() const {
        return environment;
    }

    bool operator==(const Type& other) const {
        return this == &other;
    }

    bool operator!=(const Type& other) const {
        return !(*this == other);
    }

    bool operator<(const Type& other) const {
        return name < other.name;
    }

    virtual void print(std::ostream& out) const {
        out << name;
    }

    friend std::ostream& operator<<(std::ostream& out, const Type& t) {
        return t.print(out), out;
    }

protected:
    Type(const TypeEnvironment& environment, AstQualifiedName name)
            : environment(environment), name(std::move(name)) {}

    /** A reference to the type environment this type is associated to. */
    const TypeEnvironment& environment;

    /** The name of this type. */
    AstQualifiedName name;
};

/**
 * Representing the type assigned to a constant.
 * ConstantType = NumberConstant/UnsignedConstant/FloatConstant/SymbolConstant
 */
class ConstantType : public Type {
    ConstantType(const TypeEnvironment& environment, const AstQualifiedName& name)
            : Type(environment, name) {}

    friend class TypeEnvironment;
};

/**
 * A type being a subset of another type.
 */
class SubsetType : public Type {
public:
    void print(std::ostream& out) const override;

    const Type& getBaseType() const {
        return baseType;
    }

protected:
    SubsetType(const TypeEnvironment& environment, const AstQualifiedName& name, const Type& base)
            : Type(environment, name), baseType(base) {}

private:
    friend class TypeEnvironment;

    const Type& baseType;
};

/**
 * PrimitiveType = Number/Unsigned/Float/Symbol
 * The class representing pre-built, concrete types.
 */
class PrimitiveType : public SubsetType {
public:
    void print(std::ostream& out) const override {
        out << name;
    }

private:
    PrimitiveType(const TypeEnvironment& environment, const AstQualifiedName& name, const ConstantType& base)
            : SubsetType(environment, name, base) {}

    friend class TypeEnvironment;
};

/**
 * A union type combining a list of types into a new, aggregated type.
 */
class UnionType : public Type {
public:
    void add(const Type& type);

    const std::vector<const Type*>& getElementTypes() const {
        return elementTypes;
    }

    void print(std::ostream& out) const override;

private:
    // only allow type environments to create instances
    friend class TypeEnvironment;

    /** The contained element types */
    std::vector<const Type*> elementTypes;

    UnionType(const TypeEnvironment& environment, const AstQualifiedName& name) : Type(environment, name) {}
};

/**
 * A record type combining a list of fields into a new, aggregated type.
 */
struct RecordType : public Type {
public:
    /** The type to model fields */
    struct Field {
        std::string name;  // < the name of the field
        const Type& type;  // < the type of the field
    };

    /** The list of contained fields */
    void add(const std::string& name, const Type& type);

    const std::vector<Field>& getFields() const {
        return fields;
    }

    void print(std::ostream& out) const override;

private:
    // only allow type environments to create instances
    friend class TypeEnvironment;

    std::vector<Field> fields;

    RecordType(const TypeEnvironment& environment, const AstQualifiedName& name) : Type(environment, name) {}
};

/**
 * A collection to represent sets of types. In addition to ordinary set capabilities
 * it may also represent the set of all types -- without being capable of iterating over those.
 *
 * It is the basic entity to conduct sub- and super-type computations.
 */
struct TypeSet {
public:
    using const_iterator = IterDerefWrapper<typename std::set<const Type*>::const_iterator>;

    TypeSet(bool all = false) : all(all) {}

    TypeSet(const TypeSet& other) = default;

    TypeSet(TypeSet&& other) noexcept : all(other.all), types() {
        types.swap(other.types);
    }

    template <typename... Types>
    explicit TypeSet(const Types&... types) : all(false) {
        for (const Type* cur : toVector<const Type*>(&types...)) {
            this->types.insert(cur);
        }
    }

    TypeSet& operator=(const TypeSet& other) = default;

    /** Emptiness check */
    bool empty() const {
        return !all && types.empty();
    }

    /** Universality check */
    bool isAll() const {
        return all;
    }

    /** Determines the size of this set unless it is the universal set */
    std::size_t size() const {
        assert(!all && "Unable to give size of universe.");
        return types.size();
    }

    /** Determines whether a given type is included or not */
    bool contains(const Type& type) const {
        return all || types.find(&type) != types.end();
    }

    /** Adds the given type to this set */
    void insert(const Type& type) {
        if (!all) {
            types.insert(&type);
        }
    }

    /** Calculate intersection of two TypeSet */
    static TypeSet intersection(const TypeSet& left, const TypeSet& right) {
        TypeSet result;

        if (left.isAll()) {
            return right;
        } else if (right.isAll()) {
            return left;
        }

        for (const auto& element : left) {
            if (right.contains(element)) {
                result.insert(element);
            }
        }

        return result;
    }

    /** Inserts all the types of the given set into this set */
    void insert(const TypeSet& set) {
        if (all) {
            return;
        }

        // if the other set is universal => make this one universal
        if (set.isAll()) {
            all = true;
            types.clear();
            return;
        }

        // add types one by one
        for (const auto& t : set) {
            insert(t);
        }
    }

    /** Allows to iterate over the types contained in this set (only if not universal) */
    const_iterator begin() const {
        assert(!all && "Unable to enumerate universe.");
        return derefIter(types.begin());
    }

    /** Allows to iterate over the types contained in this set (only if not universal) */
    const_iterator end() const {
        assert(!all && "Unable to enumerate universe.");
        return derefIter(types.end());
    }

    /** Determines whether this set is a subset of the given set */
    bool isSubsetOf(const TypeSet& b) const {
        if (all) {
            return b.isAll();
        }
        return all_of(*this, [&](const Type& cur) { return b.contains(cur); });
    }

    /** Determines equality between type sets */
    bool operator==(const TypeSet& other) const {
        return all == other.all && types == other.types;
    }

    /** Determines inequality between type sets */
    bool operator!=(const TypeSet& other) const {
        return !(*this == other);
    }

    /** Adds print support for type sets */
    void print(std::ostream& out) const {
        if (all) {
            out << "{ - all types - }";
        } else {
            out << "{"
                << join(types, ",", [](std::ostream& out, const Type* type) { out << type->getName(); })
                << "}";
        }
    }

    friend std::ostream& operator<<(std::ostream& out, const TypeSet& set) {
        set.print(out);
        return out;
    }

private:
    /** True if it is the all-types set, false otherwise */
    bool all;

    /** The enumeration of types in case it is not the all-types set */
    std::set<const Type*, deref_less<Type>> types;
};

/**
 * A type environment is a set of types. It's main purpose is to provide an enumeration
 * of all all types within a given program. Additionally, it manages the life cycle of
 * type instances.
 */
class TypeEnvironment {
public:
    TypeEnvironment()
            : constantTypes(initializeConstantTypes()),
              constantNumericTypes(TypeSet(
                      getType("numberConstant"), getType("unsignedConstant"), getType("floatConstant"))),
              primitiveTypes(initializePrimitiveTypes()){};

    TypeEnvironment(const TypeEnvironment&) = delete;

    virtual ~TypeEnvironment() = default;

    // -- create types in this environment --
    template <typename T, typename... Args>
    T& createType(const AstQualifiedName& name, const Args&... args) {
        assert(types.find(name) == types.end() && "Error: registering present type!");
        auto* newType = new T(*this, name, args...);
        types[name] = std::unique_ptr<Type>(newType);
        return *newType;
    }

    SubsetType& createSubsetType(const AstQualifiedName& name, TypeAttribute typeAttribute) {
        switch (typeAttribute) {
            case TypeAttribute::Signed:
                return createType<SubsetType>(name, getType("number"));
            case TypeAttribute::Unsigned:
                return createType<SubsetType>(name, getType("unsigned"));
            case TypeAttribute::Float:
                return createType<SubsetType>(name, getType("float"));
            case TypeAttribute::Symbol:
                return createType<SubsetType>(name, getType("symbol"));
            case TypeAttribute::Record:
                break;
        }

        fatal("Invalid type attribute");
    }

    bool isType(const AstQualifiedName&) const;

    bool isType(const Type& type) const;

    const Type& getType(const AstQualifiedName&) const;
    Type& getType(const AstQualifiedName&);

    const Type& getConstantType(TypeAttribute type) const {
        switch (type) {
            case TypeAttribute::Signed:
                return getType("numberConstant");
            case TypeAttribute::Unsigned:
                return getType("unsignedConstant");
            case TypeAttribute::Float:
                return getType("floatConstant");
            case TypeAttribute::Symbol:
                return getType("symbolConstant");
            case TypeAttribute::Record:
                break;
        }

        fatal("There is no constant record type");
    }

    bool isPrimitiveType(const AstQualifiedName& identifier) const {
        if (isType(identifier)) {
            return isPrimitiveType(getType(identifier));
        }
        return false;
    }

    bool isPrimitiveType(const Type& typeName) const {
        return primitiveTypes.contains(typeName);
    }

    const TypeSet& getConstantTypes() const {
        return constantTypes;
    }

    const TypeSet& getConstantNumericTypes() const {
        return constantNumericTypes;
    }

    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& out, const TypeEnvironment& environment) {
        environment.print(out);
        return out;
    }

private:
    TypeSet initializePrimitiveTypes(void);
    TypeSet initializeConstantTypes(void);

    /** The list of covered types */
    std::map<AstQualifiedName, std::unique_ptr<Type>> types;

    const TypeSet constantTypes;
    const TypeSet constantNumericTypes;

    const TypeSet primitiveTypes;
};

// ---------------------------------------------------------------
//                          Type Utilities
// ---------------------------------------------------------------

/**
 * Determines whether type a is a subtype of type b.
 */
bool isSubtypeOf(const Type& a, const Type& b);

/**
 * Returns full type qualifier for a given type
 */
std::string getTypeQualifier(const Type& type);

/**
 * Determine if a type analysis' result is equivalent to the given TypeAttribute.
 */
template <typename T>  // T = Type or T = Typeset
bool eqTypeTypeAttribute(const TypeAttribute ramType, const T& type) {
    switch (ramType) {
        case TypeAttribute::Signed:
            return isNumberType(type);
        case TypeAttribute::Unsigned:
            return isUnsignedType(type);
        case TypeAttribute::Float:
            return isFloatType(type);
        case TypeAttribute::Symbol:
            return isSymbolType(type);
        case TypeAttribute::Record:
            return isRecordType(type);
    }
    fatal("unhandled `TypeAttribute`");
}

/**
 * Convert a type analysis' type/set of type to the the TypeAttribute
 */
template <typename T>  // T = Type or T = Typeset
TypeAttribute getTypeAttribute(const T& type) {
    TypeAttribute primitiveType;
    if (isNumberType(type)) {
        primitiveType = TypeAttribute::Signed;
    } else if (isUnsignedType(type)) {
        primitiveType = TypeAttribute::Unsigned;
    } else if (isFloatType(type)) {
        primitiveType = TypeAttribute::Float;
    } else if (isRecordType(type)) {
        primitiveType = TypeAttribute::Record;
    } else if (isSymbolType(type)) {
        primitiveType = TypeAttribute::Symbol;
    } else {
        fatal("Unknown type class");
    }
    return primitiveType;
}

/**
 * Determines whether the type is numeric.
 */
template <typename T>  // T = Type or T = Typeset
inline bool isNumericType(const T& type) {
    return isFloatType(type) || isNumberType(type) || isUnsignedType(type);
}

template <typename T>  // T = Type or T = Typeset
inline bool isOrderableType(const T& type) {
    return isNumericType(type) || isSymbolType(type);
}

/**
 * Is any value in the set signed
 **/
bool hasSignedType(const TypeSet& types);

/**
 * Is any value in the set unsigned
 **/
bool hasUnsignedType(const TypeSet& types);

/**
 * Is any value in the set float
 **/
bool hasFloatType(const TypeSet& types);

/**
 * Determines whether the given type is a float type.
 */
bool isFloatType(const Type& type);

/**
 * Determines whether all the types in the given set are float types.
 */
bool isFloatType(const TypeSet& s);

/**
 * Determines whether the given type is a number type.
 */
bool isNumberType(const Type& type);

/**
 * Determines whether all the types in the given set are number types.
 */
bool isNumberType(const TypeSet& s);

/**
 * Determines whether the given type is a number type.
 */
bool isUnsignedType(const Type& type);

/**
 * Determines whether all the types in the given set are number types.
 */
bool isUnsignedType(const TypeSet& s);

/**
 * Determines whether the given type is a symbol type.
 */
bool isSymbolType(const Type& type);

/**
 * Determines whether all the types in the given set are symbol types.
 */
bool isSymbolType(const TypeSet& s);

/**
 * Determines whether the given type is a record type.
 */
bool isRecordType(const Type& type);

/**
 * Determines whether all the types in the given set are record types.
 */
bool isRecordType(const TypeSet& s);

// -- Greatest Common Sub Types --------------------------------------

/**
 * Computes the greatest common sub types of the two given types.
 */
TypeSet getGreatestCommonSubtypes(const Type& a, const Type& b);

/**
 * Computes the greatest common sub types of all the types in the given set.
 */
TypeSet getGreatestCommonSubtypes(const TypeSet& set);

/**
 * The set of pair-wise greatest common sub types of the types in the two given sets.
 */
TypeSet getGreatestCommonSubtypes(const TypeSet& a, const TypeSet& b);

/**
 * Computes the greatest common sub types of the given types.
 */
template <typename... Types>
TypeSet getGreatestCommonSubtypes(const Types&... types) {
    return getGreatestCommonSubtypes(TypeSet(types...));
}

}  // end namespace souffle
