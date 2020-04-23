/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file DebugReport.cpp
 *
 * Defines classes for creating HTML reports of debugging information.
 *
 ***********************************************************************/

#include "DebugReport.h"
#include <ostream>
#include <utility>
#include <vector>

namespace souffle {

void DebugReportSection::printIndex(std::ostream& out) const {
    out << "<a href=\"#" << id << "\">" << title << "</a>\n";
    out << "<ul>\n";
    bool isLeaf = true;
    for (const DebugReportSection& subsection : subsections) {
        if (subsection.hasSubsections()) {
            isLeaf = false;
            break;
        }
    }
    for (const DebugReportSection& subsection : subsections) {
        if (isLeaf) {
            out << "<li class='leaf'>";
        } else {
            out << "<li>";
        }
        subsection.printIndex(out);
        out << "</li>";
    }
    out << "</ul>\n";
}

void DebugReportSection::printTitle(std::ostream& out) const {
    out << "<a id=\"" << id << "\"></a>\n";
    out << "<div class='headerdiv'>\n";
    out << "<h1>" << title << "</h1>\n";
    out << "<a href='#'>(return to top)</a>\n";
    out << "</div><div style='clear:both'></div>\n";
}

void DebugReportSection::printContent(std::ostream& out) const {
    printTitle(out);
    out << "<div style='padding-left: 1em'>\n";
    out << body << "\n";
    for (const DebugReportSection& subsection : subsections) {
        subsection.printContent(out);
    }
    out << "</div>\n";
}

DebugReport::~DebugReport() {
    while (!currentSubsections.empty()) {
        endSection("forced-closed", "Forcing end of unknown section");
    }

    flush();
}

void DebugReport::flush() {
    auto&& dst = Global::config().get("debug-report");
    if (dst.empty() || empty()) return;

    std::ofstream(dst) << *this;
}

void DebugReport::addSection(std::string id, std::string title, std::string code) {
    while (true) {
        size_t i = code.find("<");
        if (i == std::string::npos) break;
        code.replace(i, 1, "&lt;");
    }

    addSection(DebugReportSection(std::move(id), std::move(title), "<pre>" + code + "</pre>\n"));
}

void DebugReport::endSection(std::string currentSectionName, std::string currentSectionTitle) {
    auto subsections = std::move(currentSubsections.top());
    currentSubsections.pop();
    addSection(DebugReportSection(
            std::move(currentSectionName), std::move(currentSectionTitle), std::move(subsections), ""));
    flush();
}

void DebugReport::print(std::ostream& out) const {
    out << "<!DOCTYPE html>\n";
    out << "<html lang='en-AU'>\n";
    out << "<head>\n";
    out << "<meta charset=\"UTF-8\">\n";
    out << "<title>Souffle Debug Report (" << Global::config().get("") << ")</title>\n";
    out << "<style>\n";
    out << "ul { list-style-type: none; }\n";
    out << "ul > li.leaf { display: inline-block; padding: 0em 1em; }\n";
    out << "ul > li.nonleaf { padding: 0em 1em; }\n";
    out << "* { font-family: sans-serif; }\n";
    out << "pre { white-space: pre-wrap; font-family: monospace; }\n";
    out << "a:link { text-decoration: none; color: blue; }\n";
    out << "a:visited { text-decoration: none; color: blue; }\n";
    out << "div.headerdiv { background-color:lightgrey; margin:10px; padding-left:10px; padding-right:10px; "
           "padding-top:3px; padding-bottom:3px; border-radius:5px }\n";
    out << ".headerdiv h1 { display:inline; }\n";
    out << ".headerdiv a { float:right; }\n";
    out << "</style>\n";
    out << "<script>\n";
    out << "function toggleVisibility(id) {\n";
    out << "  var element = document.getElementById(id);\n";
    out << "  if (element.style.display == 'none') {\n";
    out << "    element.style.display = 'block';\n";
    out << "  } else {\n";
    out << "    element.style.display = 'none';\n";
    out << "  }\n";
    out << "}\n";
    out << "</script>\n";
    out << "</head>\n";
    out << "<body>\n";
    out << "<div class='headerdiv'><h1>Souffle Debug Report (" << Global::config().get("")
        << ")</h1></div>\n";
    for (const DebugReportSection& section : sections) {
        section.printIndex(out);
    }
    for (const DebugReportSection& section : sections) {
        section.printContent(out);
    }
    out << "<a href='#'>(return to top)</a>\n";
    out << "</body>\n";
    out << "</html>\n";
}

}  // end of namespace souffle
