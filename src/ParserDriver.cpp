/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ParserDriver.cpp
 *
 * Defines the parser driver.
 *
 ***********************************************************************/

#include "ParserDriver.h"
#include "AstClause.h"
#include "AstComponent.h"
#include "AstFunctorDeclaration.h"
#include "AstIO.h"
#include "AstPragma.h"
#include "AstProgram.h"
#include "AstQualifiedName.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "DebugReport.h"
#include "ErrorReport.h"
#include "Util.h"
#include <memory>
#include <utility>

using YY_BUFFER_STATE = struct yy_buffer_state*;
extern YY_BUFFER_STATE yy_scan_string(const char*, yyscan_t scanner);
extern int yylex_destroy(yyscan_t scanner);
extern int yylex_init_extra(scanner_data* data, yyscan_t* scanner);
extern void yyset_in(FILE* in_str, yyscan_t scanner);

namespace souffle {

ParserDriver::ParserDriver() = default;

ParserDriver::~ParserDriver() = default;

std::unique_ptr<AstTranslationUnit> ParserDriver::parse(
        const std::string& filename, FILE* in, ErrorReport& errorReport, DebugReport& debugReport) {
    translationUnit =
            std::make_unique<AstTranslationUnit>(std::make_unique<AstProgram>(), errorReport, debugReport);
    yyscan_t scanner;
    scanner_data data;
    data.yyfilename = filename;
    yylex_init_extra(&data, &scanner);
    yyset_in(in, scanner);

    yy::parser parser(*this, scanner);
    parser.parse();

    yylex_destroy(scanner);

    return std::move(translationUnit);
}

std::unique_ptr<AstTranslationUnit> ParserDriver::parse(
        const std::string& code, ErrorReport& errorReport, DebugReport& debugReport) {
    translationUnit =
            std::make_unique<AstTranslationUnit>(std::make_unique<AstProgram>(), errorReport, debugReport);

    scanner_data data;
    data.yyfilename = "<in-memory>";
    yyscan_t scanner;
    yylex_init_extra(&data, &scanner);
    yy_scan_string(code.c_str(), scanner);
    yy::parser parser(*this, scanner);
    parser.parse();

    yylex_destroy(scanner);

    return std::move(translationUnit);
}

std::unique_ptr<AstTranslationUnit> ParserDriver::parseTranslationUnit(
        const std::string& filename, FILE* in, ErrorReport& errorReport, DebugReport& debugReport) {
    ParserDriver parser;
    return parser.parse(filename, in, errorReport, debugReport);
}

std::unique_ptr<AstTranslationUnit> ParserDriver::parseTranslationUnit(
        const std::string& code, ErrorReport& errorReport, DebugReport& debugReport) {
    ParserDriver parser;
    return parser.parse(code, errorReport, debugReport);
}

void ParserDriver::addPragma(std::unique_ptr<AstPragma> p) {
    translationUnit->getProgram()->addPragma(std::move(p));
}

void ParserDriver::addFunctorDeclaration(std::unique_ptr<AstFunctorDeclaration> f) {
    const std::string& name = f->getName();
    if (const AstFunctorDeclaration* prev = getFunctorDeclaration(*translationUnit->getProgram(), name)) {
        Diagnostic err(Diagnostic::ERROR,
                DiagnosticMessage("Redefinition of functor " + toString(name), f->getSrcLoc()),
                {DiagnosticMessage("Previous definition", prev->getSrcLoc())});
        translationUnit->getErrorReport().addDiagnostic(err);
    } else {
        translationUnit->getProgram()->addFunctorDeclaration(std::move(f));
    }
}

void ParserDriver::addRelation(std::unique_ptr<AstRelation> r) {
    const auto& name = r->getQualifiedName();
    if (AstRelation* prev = getRelation(*translationUnit->getProgram(), name)) {
        Diagnostic err(Diagnostic::ERROR,
                DiagnosticMessage("Redefinition of relation " + toString(name), r->getSrcLoc()),
                {DiagnosticMessage("Previous definition", prev->getSrcLoc())});
        translationUnit->getErrorReport().addDiagnostic(err);
    } else {
        translationUnit->getProgram()->addRelation(std::move(r));
    }
}

void ParserDriver::addIO(std::unique_ptr<AstIO> d) {
    if (d->getType() == AstIO::PrintsizeIO) {
        for (const auto& cur : translationUnit->getProgram()->getIOs()) {
            if (cur->getQualifiedName() == d->getQualifiedName() && cur->getType() == AstIO::PrintsizeIO) {
                Diagnostic err(Diagnostic::ERROR,
                        DiagnosticMessage("Redefinition of printsize directives for relation " +
                                                  toString(d->getQualifiedName()),
                                d->getSrcLoc()),
                        {DiagnosticMessage("Previous definition", cur->getSrcLoc())});
                translationUnit->getErrorReport().addDiagnostic(err);
                return;
            }
        }
    }
    translationUnit->getProgram()->addIO(std::move(d));
}

void ParserDriver::addType(std::unique_ptr<AstType> type) {
    const auto& name = type->getQualifiedName();
    if (const AstType* prev = getType(*translationUnit->getProgram(), name)) {
        Diagnostic err(Diagnostic::ERROR,
                DiagnosticMessage("Redefinition of type " + toString(name), type->getSrcLoc()),
                {DiagnosticMessage("Previous definition", prev->getSrcLoc())});
        translationUnit->getErrorReport().addDiagnostic(err);
    } else {
        translationUnit->getProgram()->addType(std::move(type));
    }
}

void ParserDriver::addClause(std::unique_ptr<AstClause> c) {
    translationUnit->getProgram()->addClause(std::move(c));
}
void ParserDriver::addComponent(std::unique_ptr<AstComponent> c) {
    translationUnit->getProgram()->addComponent(std::move(c));
}
void ParserDriver::addInstantiation(std::unique_ptr<AstComponentInit> ci) {
    translationUnit->getProgram()->addInstantiation(std::move(ci));
}

std::vector<std::unique_ptr<AstClause>> ParserDriver::toClauseBodies(const AstBody& body) {
    // collect clause results
    std::vector<std::unique_ptr<AstClause>> bodies;
    for (auto&& cnf : body.getDisjuncts()) {
        bodies.push_back(std::make_unique<AstClause>());
        bodies.back()->setBodyLiterals(souffle::clone(cnf));
    }

    return bodies;
}

void ParserDriver::warning(const SrcLocation& loc, const std::string& msg) {
    translationUnit->getErrorReport().addWarning(msg, loc);
}
void ParserDriver::error(const SrcLocation& loc, const std::string& msg) {
    translationUnit->getErrorReport().addError(msg, loc);
}
void ParserDriver::error(const std::string& msg) {
    translationUnit->getErrorReport().addDiagnostic(Diagnostic(Diagnostic::ERROR, DiagnosticMessage(msg)));
}

}  // end of namespace souffle
