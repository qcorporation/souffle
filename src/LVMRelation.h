/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file LVMRelation.h
 *
 * Defines LVM Relations
 *
 ***********************************************************************/

#pragma once

#include "LVMIndex.h"
#include "ParallelUtils.h"
#include "RamIndexAnalysis.h"
#include "RamTypes.h"
#include "Relation.h"

#include <deque>
#include <map>
#include <memory>
#include <utility>
#include <vector>

namespace souffle {

/**
 * A relation, composed of a collection of indexes.
 */
class LVMRelation {
public:
    /**
     * Creates a relation, build all necessary indexes.
     */
    LVMRelation(std::size_t arity, const std::string& name, std::vector<std::string>&& attributeTypes,
            const MinIndexSelection& orderSet, IndexFactory factory = &createBTreeIndex);

    LVMRelation(LVMRelation& other) = delete;

    virtual ~LVMRelation() {}

    /**
     * Drops an index from the maintained indexes. All but one index
     * may be removed.
     */
    void removeIndex(const size_t& indexPos);

    /**
     * Add the given tuple to this relation.
     */
    virtual bool insert(const TupleRef& tuple);

    /**
     * Add the given tuple to this relation.
     */
    bool insert(const RamDomain* tuple) {
        return insert(TupleRef(tuple, arity));
    }

    /**
     * Add all entries of the given relation to this relation.
     */
    void insert(const LVMRelation& other);

    /**
     * Tests whether this relation contains the given tuple.
     */
    bool contains(const TupleRef& tuple) const;

    /**
     * Obtains a stream to scan the entire relation.
     */
    Stream scan() const;

    /**
     * Obtains a stream covering the interval between the two given entries.
     */
    Stream range(const size_t& indexPos, const TupleRef& low, const TupleRef& high) const;

    /**
     * Removes the content of this relation, but retains the empty indexes.
     */
    void clear();

    /**
     * Swaps the content of this and the given relation, including the
     * installed indexes.
     */
    void swap(LVMRelation& other);

    /**
     * Set level
     */
    void setLevel(size_t level) {
        this->level = level;  // TODO necessary?
    }

    /**
     * Return the level of the relation.
     */
    size_t getLevel() const;

    /**
     * Return the relation name.
     */
    const std::string& getName() const;

    /**
     * Return the attribute types
     */
    const std::vector<std::string>& getAttributeTypeQualifiers() const;

    /**
     * Return arity
     */
    size_t getArity() const;

    /**
     * Return number of tuples in relation (full-order)
     */
    size_t size() const;

    /**
     * Check if the relation is empty
     */
    bool empty() const;

    /**
     * Clear all indexes
     */
    void purge();

    /**
     * Check if a tuple exists in realtion
     */
    bool exists(const TupleRef& tuple) const;

    /**
     * Extend another relation
     */
    virtual void extend(const LVMRelation& rel);

protected:
    // Relation name
    std::string relName;

    // Relation Arity
    size_t arity;

    // Relation attributes types
    std::vector<std::string> attributeTypes;

    // a map of managed indexes
    std::vector<std::unique_ptr<Index>> indexes;

    // a pointer to the main index within the managed index
    Index* main;

    // relation level
    size_t level;
};

/**
 * Interpreter Equivalence Relation
 */

class LVMEqRelation : public LVMRelation {
public:
    LVMEqRelation(size_t arity, const std::string& relName, std::vector<std::string>&& attributeTypes,
            const MinIndexSelection& orderSet);

    /** Insert tuple */
    bool insert(const TupleRef& tuple) override;
    /** Find the new knowledge generated by inserting a tuple */
    std::vector<RamDomain*> extend(const TupleRef& tuple);
    /** Extend this relation with new knowledge generated by inserting all tuples from a relation */
    void extend(const LVMRelation& rel) override;
};

}  // end of namespace souffle
