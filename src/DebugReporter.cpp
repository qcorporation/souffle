/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file DebugReporter.cpp
 *
 * Defines class for adapting other transformers to produce debug output
 *
 ***********************************************************************/

#include "DebugReporter.h"
#include "AstTranslationUnit.h"
#include "AstTypeAnalysis.h"
#include "AstTypeEnvironmentAnalysis.h"
#include "AstUtils.h"
#include "DebugReport.h"
#include "PrecedenceGraph.h"
#include <chrono>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <utility>

namespace souffle {

bool DebugReporter::transform(AstTranslationUnit& translationUnit) {
    translationUnit.getDebugReport().startSection();
    auto datalogSpecOriginal = pprint(*translationUnit.getProgram());
    auto start = std::chrono::high_resolution_clock::now();
    bool changed = applySubtransformer(translationUnit, wrappedTransformer.get());
    auto end = std::chrono::high_resolution_clock::now();

    if (changed) {
        generateDebugReport(translationUnit, datalogSpecOriginal);
    }

    auto elapsed = std::to_string(std::chrono::duration<double>(end - start).count());
    translationUnit.getDebugReport().endSection(wrappedTransformer->getName(),
            wrappedTransformer->getName() + " (" + elapsed + "s)" + (changed ? "" : " (unchanged)"));
    return changed;
}

DebugReportSection formatCodeSection(const std::string& id, const std::string& title, std::string code) {
    std::stringstream codeHTML;
    std::string escapedCode = std::move(code);
    while (true) {
        size_t i = escapedCode.find("<");
        if (i == std::string::npos) {
            break;
        }
        escapedCode.replace(i, 1, "&lt;");
    }
    codeHTML << "<pre>" << escapedCode << "</pre>\n";
    return DebugReportSection(id, title, codeHTML.str());
}

void DebugReporter::generateDebugReport(AstTranslationUnit& tu, const std::string& preTransformDatalog) {
    auto datalogSpec = pprint(*tu.getProgram());
    tu.getDebugReport().addSection("dl", "Datalog",
            preTransformDatalog.empty() ? std::move(datalogSpec)
                                        : generateDiff(preTransformDatalog, datalogSpec));
}

}  // end of namespace souffle
