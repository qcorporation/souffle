/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstConstraintAnalysis.h
 *
 * Contains AST Constraint Analysis Infrastructure for doing constraint analysis on AST objects
 *
 ***********************************************************************/

#pragma once

#include "AstArgument.h"
#include "AstClause.h"
#include "AstVisitor.h"
#include "Constraints.h"
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <utility>

namespace souffle {

/**
 * A variable type to be utilized by AST constraint analysis. Each such variable is
 * associated with an AstArgument which's property it is describing.
 *
 * @tparam PropertySpace the property space associated to the analysis
 */
template <typename PropertySpace>
struct AstConstraintAnalysisVar : public Variable<const AstArgument*, PropertySpace> {
    explicit AstConstraintAnalysisVar(const AstArgument* arg)
            : Variable<const AstArgument*, PropertySpace>(arg) {}
    explicit AstConstraintAnalysisVar(const AstArgument& arg)
            : Variable<const AstArgument*, PropertySpace>(&arg) {}

    /** adds print support */
    void print(std::ostream& out) const override {
        out << "var(" << *(this->id) << ")";
    }
};

/**
 * A base class for AstConstraintAnalysis collecting constraints for an analysis
 * by visiting every node of a given AST. The collected constraints are
 * then utilized to obtain the desired analysis result.
 *
 * @tparam AnalysisVar the type of variable (and included property space)
 *      to be utilized by this analysis.
 */
template <typename AnalysisVar>
class AstConstraintAnalysis : public AstVisitor<void> {
public:
    using value_type = typename AnalysisVar::property_space::value_type;
    using constraint_type = std::shared_ptr<Constraint<AnalysisVar>>;
    using solution_type = std::map<const AstArgument*, value_type>;

    virtual void collectConstraints(const AstClause& clause) {
        visitDepthFirstPreOrder(clause, *this);
    }

    /**
     * Runs this constraint analysis on the given clause.
     *
     * @param clause the close to be analysed
     * @param debug a flag enabling the printing of debug information
     * @return an assignment mapping a property to each argument in the given clause
     */
    solution_type analyse(const AstClause& clause, std::ostream* debugOutput = nullptr) {
        collectConstraints(clause);

        assignment = constraints.solve();

        // print debug information if desired
        if (debugOutput != nullptr) {
            *debugOutput << "Clause: " << clause << "\n";
            *debugOutput << "Problem:\n" << constraints << "\n";
            *debugOutput << "Solution:\n" << assignment << "\n";
        }

        // convert assignment to result
        solution_type solution;
        visitDepthFirst(clause, [&](const AstArgument& arg) { solution[&arg] = assignment[getVar(arg)]; });
        return solution;
    }

protected:
    /**
     * A utility function mapping an AstArgument to its associated analysis variable.
     *
     * @param arg the AST argument to be mapped
     * @return the analysis variable representing its associated value
     */
    AnalysisVar getVar(const AstArgument& arg) {
        const auto* var = dynamic_cast<const AstVariable*>(&arg);
        if (var == nullptr) {
            // no mapping required
            return AnalysisVar(arg);
        }

        auto it = var2var.find(var);
        if (it != var2var.end()) return it->second;

        // filter through map => always take the same variable
        auto&& [scope_it, fresh] = varScopes.back().insert({var->getName(), AnalysisVar(var)});
        auto&& [name, analysisVar] = *scope_it;
        if (fresh) onIntroducedVar(name, analysisVar);
        var2var.insert({var, analysisVar});
        return analysisVar;
    }

    // This is an ugly hack to allow groundness analysis to add imply binding
    // when adding a var in an inner scope.
    virtual void onIntroducedVar(const std::string& /*name*/, AnalysisVar&) {}

    /**
     * A utility function mapping an AstArgument to its associated analysis variable.
     *
     * @param arg the AST argument to be mapped
     * @return the analysis variable representing its associated value
     */
    AnalysisVar getVar(const AstArgument* arg) {
        return getVar(*arg);
    }

    /** Adds another constraint to the internally maintained list of constraints */
    void addConstraint(const constraint_type& constraint) {
        constraints.add(constraint);
    }

    Assignment<AnalysisVar> assignment;

    /** The list of constraints making underlying this analysis */
    Problem<AnalysisVar> constraints;

    /** A map mapping variables to unique instances to facilitate the unification of variables */
    // variable -> analysis variable (vars w/ same name in a scope map to same analysis var)
    std::map<const AstArgument*, AnalysisVar> var2var;
    using VarName2AnalysisVar = std::map<std::string, AnalysisVar>;
    std::vector<VarName2AnalysisVar> varScopes = {{}};

    VarName2AnalysisVar& scopePush() {
        varScopes.push_back({});
        return *(varScopes.end() - 2);
    }

    VarName2AnalysisVar scopePop() {
        assert(1 < varScopes.size() && "cannot pop top-level scope");
        auto popped = std::move(varScopes.back());
        varScopes.pop_back();
        return popped;
    }
};

}  // end of namespace souffle
