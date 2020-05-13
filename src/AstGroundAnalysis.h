/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstGroundAnalysis.h
 *
 * Defines a function for computing the grounded arguments in a clause
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include <map>
#include <ostream>
#include <sstream>

namespace souffle {

class AstArgument;
class AstClause;
class AstTranslationUnit;

class GroundAnalysis : public AstAnalysis {
public:
    static constexpr const char* name = "ground-analysis";

    GroundAnalysis() : AstAnalysis(name) {}

    void run(const AstTranslationUnit& translationUnit) override;
    void run(const AstTranslationUnit& translationUnit, const AstClause& clause);

    void print(std::ostream& os) const override {
        os << analysisLogs.str();
    }

    /**
     * Get the computed types for the given argument.
     */
    bool isGrounded(const AstArgument* argument) const {
        return grounded.at(argument);
    }

    /**
     * Analyse the given clause and computes for each contained argument
     * whether it is a grounded value or not.
     *
     * @param tu the translation unit containing the clause
     * @param clause the clause to be analyzed
     * @return a map mapping each contained argument to a boolean indicating
     *      whether the argument represents a grounded value or not
     */
    static std::map<const AstArgument*, bool> getGroundedTerms(
            const AstTranslationUnit& tu, const AstClause& clause, std::ostream* /*logs*/ = nullptr);

private:
    std::map<const AstArgument*, bool> grounded;
    std::stringstream analysisLogs;
};

}  // end of namespace souffle
