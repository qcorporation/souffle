/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReadStreamCSV.h
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include "ReadStream.h"
#include "SymbolTable.h"
#include "utility/ContainerUtil.h"
#include "utility/FileUtil.h"
#include "utility/StringUtil.h"

#ifdef USE_LIBZ
#include "gzfstream.h"
#else
#include <fstream>
#endif

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <unordered_set>

namespace souffle {
class RecordTable;

class ReadStreamCSV : public ReadStream {
public:
    /* Only allow comma, pipe and tabs as delimiters */
    const std::unordered_set<char> DELIMETERS = {',', '|', '\t'};

    ReadStreamCSV(std::istream& file, const std::map<std::string, std::string>& rwOperation,
            SymbolTable& symbolTable, RecordTable& recordTable)
            : ReadStream(rwOperation, symbolTable, recordTable),
              delimiter(getOr(rwOperation, "delimiter", "\t")), file(file), lineNumber(0),
              inputMap(getInputColumnMap(rwOperation, arity)) {

        if (delimiter.length() > 1) {
            fatal("Delimiter can only be 1 character long, given:%s", delimiter);
        }

        if (DELIMETERS.find(delimiter[0]) == DELIMETERS.end()) {
            fatal("Unsuported delimiter: '%c'", delimiter[0]);
        } else {
            DELIMITER = delimiter[0];
        }

        while (inputMap.size() < arity) {
            int size = static_cast<int>(inputMap.size());
            inputMap[size] = size;
        }
    }

protected:
    /**
     * Read and return the next tuple.
     *
     * Returns nullptr if no tuple was readable.
     * @return
     */
    std::unique_ptr<RamDomain[]> readNextTuple() override {
        if (file.eof()) {
            return nullptr;
        }
        std::string line;
        std::unique_ptr<RamDomain[]> tuple = std::make_unique<RamDomain[]>(typeAttributes.size());

        if (!getline(file, line)) {
            return nullptr;
        }

        std::string_view line_view{line.c_str(), line.length()};

        // Handle Windows line endings on non-Windows systems
        if (!line_view.empty() && line_view.back() == '\r') {
            line_view = line_view.substr(0, line_view.length() - 1);
        }
        ++lineNumber;

        size_t index = 0;
        size_t columnsFilled = 0;

        while (index < line_view.length() && (columnsFilled < arity)) {
            size_t charactersRead = 0;
            std::string_view element;

            try {
                auto&& ty = typeAttributes.at(inputMap[columnsFilled]);
                switch (ty[0]) {
                    case 's': {
                        element = readSymbolValue(line_view, index);
                        tuple[inputMap[columnsFilled]] = symbolTable.unsafeLookup(std::string(element));
                        charactersRead = element.size();
                        break;
                    }
                    case 'r': {
                        element = readRecordValue(line_view, index);
                        tuple[inputMap[columnsFilled]] = readRecord(element, ty, 0, &charactersRead);
                        break;
                    }
                    case 'i': {
                        element = readNumberValue(line_view, index);
                        tuple[inputMap[columnsFilled]] = RamSignedFromString(std::string(element), &charactersRead);
                        break;
                    }
                    case 'u': {
                        element = readNumberValue(line_view, index);
                        tuple[inputMap[columnsFilled]] = ramBitCast(readRamUnsigned(std::string(element), charactersRead));
                        break;
                    }
                    case 'f': {
                        element = readNumberValue(line_view, index);
                        tuple[inputMap[columnsFilled]] = ramBitCast(RamFloatFromString(std::string(element), &charactersRead));
                        break;
                    }
                    default: fatal("invalid type attribute: `%c`", ty[0]);
                }
                // Check if everything was read.
                if (charactersRead != element.size()) {
                    throw std::invalid_argument(
                            "Expected: " + delimiter + " or \\n. Got: " + element[charactersRead]);
                }
            } catch (...) {
                std::stringstream errorMessage;
                errorMessage << "Error converting <" + std::string(element) + "> in column " << columnsFilled + 1 << " in line "
                             << lineNumber << "; ";
                throw std::invalid_argument(errorMessage.str());
            }
            ++columnsFilled;
        }

        // TODO: Should we allow empty columns??
        /* If last element is a delimeter, last element is empty; therefore count it as a column */
        if (line_view[index-1] == DELIMITER) {
            auto&& ty = typeAttributes.at(inputMap[columnsFilled]);
            if (ty[0] != 's') {
                std::stringstream errorMessage;
                errorMessage << "Only symbols can have empty columns. Found at arity: " 
                            << columnsFilled << " in line " << lineNumber << "; ";
                throw std::invalid_argument(errorMessage.str());
            }
            tuple[inputMap[columnsFilled]] = symbolTable.unsafeLookup("");
            ++columnsFilled;
        }

        if (columnsFilled != arity) {
            std::stringstream errorMessage;
            errorMessage << "Arity count does not match expected. Expected: " << arity << ", found: " 
                        << columnsFilled << " in line " << lineNumber << "; ";
            throw std::invalid_argument(errorMessage.str());
        } else if (index < (line.length() - 1)) {
            std::stringstream errorMessage;
            errorMessage << "Line not fully processed. Index must be pointing at the end of line but got: " 
                        << index << ", line length: " << line.length() << ", current column: " 
                        << columnsFilled << " in line " << lineNumber << "; ";
            throw std::invalid_argument(errorMessage.str());
        }

        return tuple;
    }

    /**
     * Read an unsigned element. Possible bases are 2, 10, 16
     * Base is indicated by the first two chars.
     */
    RamUnsigned readRamUnsigned(const std::string& element, size_t& charactersRead) {
        // Sanity check
        assert(element.size() > 0);

        RamSigned value = 0;

        // Check prefix and parse the input.
        if (isPrefix("0b", element)) {
            value = RamUnsignedFromString(element, &charactersRead, 2);
        } else if (isPrefix("0x", element)) {
            value = RamUnsignedFromString(element, &charactersRead, 16);
        } else {
            value = RamUnsignedFromString(element, &charactersRead);
        }
        return value;
    }

    std::string_view readRecordValue(const std::string_view& line, size_t& index) {
        std::string_view element;
        bool escaped = false; // TODO: do we actually need it? Will we ever escape square brackets?

        if (line[index] != '[') {
            std::stringstream errorMessage;
            errorMessage << "Unbalanced record parenthesis " << lineNumber << "; ";
            throw std::invalid_argument(errorMessage.str());
        }

        int record_parens = 0;
        size_t next_index = index;
        /* Need to search character by character for escape chars */
        while (next_index < line.length()) {
            if (line[next_index] == '\\') {
                next_index++;
                escaped = true;
            }

            if (line[next_index] == ']' && !escaped) {
                record_parens--;
            } else if (line[next_index] == '[' && !escaped) {
                record_parens++;
            }

            // Check for unbalanced parenthesis.
            if (record_parens < 0) {
                break;
            };

            if ((line[next_index] == DELIMITER) && !escaped && (record_parens == 0)) {
                break;
            }
            escaped = false;
            next_index++;
        }

        if (record_parens != 0) {
            std::stringstream errorMessage;
            errorMessage << "Unbalanced record parenthesis " << lineNumber << "; ";
            throw std::invalid_argument(errorMessage.str());
        }

        element = line.substr(index, next_index - index);
        index = next_index + 1;

        return element;
    }

    std::string_view readSymbolValue(const std::string_view& line, size_t& index) {
        std::string_view element;

        /* Allow quoted symbols */
        size_t next_index = index;
        bool quoted_symbol_region = false;

        if (line[next_index] == '\"') {
            quoted_symbol_region = true;
            next_index++;
        }
        /* Need to search character by character for escape chars */
        while (next_index < line.length()) {
            /* Excape everything */
            if (line[next_index] == '\\') {
                next_index += 2;
                continue;
            }
            
            /* Takes care of ending quote in symbol. Escaping quotes with backslashes \" and 
             * escaping quotes with quotes "" .
             * If sequence such as \"" is found, the backslash will escape the first quote and 
             *    the second quote will denote the end of the symbol
             */
            if (line[next_index] == '\"' && quoted_symbol_region) {
                /* Is this quote escaping another quote or is it the end of the symbol? */
                next_index++;
                if ((next_index < line.length()) && line[next_index] == '\"') {
                    next_index ++;
                    continue;
                }

                quoted_symbol_region = false;
                break;
            }

            if (line[next_index] == DELIMITER && !quoted_symbol_region) {
                break;
            }
            next_index++;
        }

        if (quoted_symbol_region) {
            std::stringstream errorMessage;
            errorMessage << "Bad parsing configuration found while reading symbol: in line: " << line << 
                    "; line number: " << lineNumber << "; at index: " << next_index;
            throw std::invalid_argument(errorMessage.str());
        }
        
        element = line.substr(index, next_index - index);
        index = next_index + 1;

        return element;
    }

    std::string_view readNumberValue(const std::string_view& line, size_t& index) {
        std::string_view element;

        size_t next_index = line.find(DELIMITER, index);
        /* No need to check for escape character here since only numeric characters are expected */
        element = line.substr(index, next_index - index);
        index = (next_index == std::string::npos) ? line.length() + 1 : (next_index + 1);

        return element;
    }

    std::map<int, int> getInputColumnMap(
            const std::map<std::string, std::string>& rwOperation, const unsigned arity_) const {
        std::string columnString = getOr(rwOperation, "columns", "");
        std::map<int, int> inputColumnMap;

        if (!columnString.empty()) {
            std::istringstream iss(columnString);
            std::string mapping;
            int index = 0;
            while (std::getline(iss, mapping, ':')) {
                inputColumnMap[stoi(mapping)] = index++;
            }
            if (inputColumnMap.size() < arity_) {
                throw std::invalid_argument("Invalid column set was given: <" + columnString + ">");
            }
        } else {
            while (inputColumnMap.size() < arity_) {
                int size = static_cast<int>(inputColumnMap.size());
                inputColumnMap[size] = size;
            }
        }
        return inputColumnMap;
    }

    const std::string delimiter;
    char DELIMITER;
    std::istream& file;
    size_t lineNumber;
    std::map<int, int> inputMap;
};

class ReadFileCSV : public ReadStreamCSV {
public:
    ReadFileCSV(const std::map<std::string, std::string>& rwOperation, SymbolTable& symbolTable,
            RecordTable& recordTable)
            : ReadStreamCSV(fileHandle, rwOperation, symbolTable, recordTable),
              baseName(souffle::baseName(getFileName(rwOperation))),
              fileHandle(getFileName(rwOperation), std::ios::in | std::ios::binary) {
        if (!fileHandle.is_open()) {
            throw std::invalid_argument("Cannot open fact file " + baseName + "\n");
        }
        // Strip headers if we're using them
        if (getOr(rwOperation, "headers", "false") == "true") {
            std::string line;
            getline(file, line);
        }
    }

    /**
     * Read and return the next tuple.
     *
     * Returns nullptr if no tuple was readable.
     * @return
     */
    std::unique_ptr<RamDomain[]> readNextTuple() override {
        try {
            return ReadStreamCSV::readNextTuple();
        } catch (std::exception& e) {
            std::stringstream errorMessage;
            errorMessage << e.what();
            errorMessage << "cannot parse fact file " << baseName << "!\n";
            throw std::invalid_argument(errorMessage.str());
        }
    }

    ~ReadFileCSV() override = default;

protected:
    static std::string getFileName(const std::map<std::string, std::string>& rwOperation) {
        return getOr(rwOperation, "filename", rwOperation.at("name") + ".facts");
    }

    std::string baseName;
#ifdef USE_LIBZ
    gzfstream::igzfstream fileHandle;
#else
    std::ifstream fileHandle;
#endif
};

class ReadCinCSVFactory : public ReadStreamFactory {
public:
    std::unique_ptr<ReadStream> getReader(const std::map<std::string, std::string>& rwOperation,
            SymbolTable& symbolTable, RecordTable& recordTable) override {
        return std::make_unique<ReadStreamCSV>(std::cin, rwOperation, symbolTable, recordTable);
    }

    const std::string& getName() const override {
        static const std::string name = "stdin";
        return name;
    }
    ~ReadCinCSVFactory() override = default;
};

class ReadFileCSVFactory : public ReadStreamFactory {
public:
    std::unique_ptr<ReadStream> getReader(const std::map<std::string, std::string>& rwOperation,
            SymbolTable& symbolTable, RecordTable& recordTable) override {
        return std::make_unique<ReadFileCSV>(rwOperation, symbolTable, recordTable);
    }

    const std::string& getName() const override {
        static const std::string name = "file";
        return name;
    }

    ~ReadFileCSVFactory() override = default;
};

} /* namespace souffle */
