/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstVisitor.h
 *
 * Provides some infrastructure for the implementation of operations
 * on top of AST structures.
 *
 ***********************************************************************/

#pragma once

#include "AstArgument.h"
#include "AstAttribute.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstType.h"

#include <functional>
#include <memory>
#include <utility>
#include <vector>

namespace souffle {

/** A tag type required for the is_ast_visitor type trait to identify AstVisitors */
struct ast_visitor_tag {};

/**
 * The generic base type of all AstVisitors realizing the dispatching of
 * visitor calls. Each visitor may define a return type R and a list of
 * extra parameters to be passed along with the visited AstNodes to the
 * corresponding visitor function.
 *
 * @tparam R the result type produced by a visit call
 * @tparam Params extra parameters to be passed to the visit call
 */
template <typename R = void, typename... Params>
struct AstVisitor : public ast_visitor_tag {
    /** A virtual destructor */
    virtual ~AstVisitor() = default;

    /** The main entry for the user allowing visitors to be utilized as functions */
    R operator()(const AstNode& node, Params... args) {
        return visit(node, args...);
    }

    /**
     * The main entry for a visit process conducting the dispatching of
     * a visit to the various sub-types of AstNodes. Sub-classes may override
     * this implementation to conduct pre-visit operations.
     *
     * @param node the node to be visited
     * @param args a list of extra parameters to be forwarded
     */
    virtual R visit(const AstNode& node, Params... args) {
        // dispatch node processing based on dynamic type

#define GO(Kind, Parent) \
    if (const auto* n = dynamic_cast<const Ast##Kind*>(&node)) return visit##Kind(*n, args...);

#include "AstVisitor.inl"

#undef GO

        // did not work ...
        fatal("unsupported type: %s", typeid(node).name());
    }

    /**
     * The main entry for a visit process conducting the dispatching of
     * a visit to the various sub-types of AstNodes. Sub-classes may override
     * this implementation to conduct post-visit operations.
     *
     * @param node the node to be visited
     * @param args a list of extra parameters to be forwarded
     */
    virtual void leave(const AstNode& node, Params... args) {
        // dispatch node processing based on dynamic type

#define GO(Kind, Parent) \
    if (const auto* n = dynamic_cast<const Ast##Kind*>(&node)) return leave##Kind(*n, args...);

#include "AstVisitor.inl"

#undef GO

        // did not work ...
        fatal("unsupported type: %s", typeid(node).name());
    }

protected:
#define CALL_LEAVE(Node)
#define GO(Kind, Parent)                                           \
    virtual R visit##Kind(const Ast##Kind& n, Params... args) {    \
        return visit##Parent(n, args...);                          \
    }                                                              \
    virtual void leave##Kind(const Ast##Kind& n, Params... args) { \
        leave##Parent(n, args...);                                 \
    }

#include "AstVisitor.inl"

#undef GO

    /** The base case for all visitors -- if no more specific overload was defined */
    virtual R visitNode(const AstNode& /*node*/, Params... /*args*/) {
        return R();
    }

    virtual void leaveNode(const AstNode& /*node*/, Params... /*args*/) {}
};  // namespace souffle

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first pre-order fashion applying the given visitor to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param visitor the visitor to be applied on each node
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename R, typename... Ps, typename... Args>
void visitDepthFirstPreOrder(const AstNode& root, AstVisitor<R, Ps...>& visitor, Args&... args) {
    visitor(root, args...);
    for (const AstNode* cur : root.getChildNodes()) {
        if (cur != nullptr) {
            visitDepthFirstPreOrder(*cur, visitor, args...);
        }
    }
    visitor.leave(root, args...);
}

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first post-order fashion applying the given visitor to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param visitor the visitor to be applied on each node
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename R, typename... Ps, typename... Args>
void visitDepthFirstPostOrder(const AstNode& root, AstVisitor<R, Ps...>& visitor, Args&... args) {
    for (const AstNode* cur : root.getChildNodes()) {
        if (cur != nullptr) {
            visitDepthFirstPostOrder(*cur, visitor, args...);
        }
    }
    visitor(root, args...);
}

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first pre-order fashion applying the given visitor to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param visitor the visitor to be applied on each node
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename R, typename... Ps, typename... Args>
void visitDepthFirst(const AstNode& root, AstVisitor<R, Ps...>& visitor, Args&... args) {
    visitDepthFirstPreOrder(root, visitor, args...);
}

namespace detail {

/**
 * A specialized visitor wrapping a lambda function -- an auxiliary type required
 * for visitor convenience functions.
 */
template <typename R, typename N>
struct LambdaAstVisitor : public AstVisitor<void> {
    std::function<R(const N&)> lambda;
    LambdaAstVisitor(std::function<R(const N&)> lambda) : lambda(std::move(lambda)) {}
    void visit(const AstNode& node) override {
        if (const auto* n = dynamic_cast<const N*>(&node)) {
            lambda(*n);
        }
    }
};

/**
 * A factory function for creating LambdaAstVisitor instances.
 */
template <typename R, typename N>
LambdaAstVisitor<R, N> makeLambdaAstVisitor(const std::function<R(const N&)>& fun) {
    return LambdaAstVisitor<R, N>(fun);
}

/**
 * A type trait determining whether a given type is a visitor or not.
 */
template <typename T>
struct is_ast_visitor {
    static constexpr size_t value = std::is_base_of<ast_visitor_tag, T>::value;
};

template <typename T>
struct is_ast_visitor<const T> : public is_ast_visitor<T> {};

template <typename T>
struct is_ast_visitor<T&> : public is_ast_visitor<T> {};
}  // namespace detail

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first pre-order fashion applying the given function to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename R, typename N>
void visitDepthFirst(const AstNode& root, const std::function<R(const N&)>& fun) {
    auto visitor = detail::makeLambdaAstVisitor(fun);
    visitDepthFirst<void>(root, visitor);
}

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first pre-order fashion applying the given function to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename Lambda, typename R = typename lambda_traits<Lambda>::result_type,
        typename N = typename lambda_traits<Lambda>::arg0_type>
typename std::enable_if<!detail::is_ast_visitor<Lambda>::value, void>::type visitDepthFirst(
        const AstNode& root, const Lambda& fun) {
    visitDepthFirst(root, std::function<R(const N&)>(fun));
}

/**
 * A utility function visiting all nodes within a given list of AST root nodes
 * recursively in a depth-first pre-order fashion applying the given function to each
 * encountered node.
 *
 * @param list the list of roots of the ASTs to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename T, typename Lambda>
void visitDepthFirst(const std::vector<T*>& list, const Lambda& fun) {
    for (const auto& cur : list) {
        visitDepthFirst(*cur, fun);
    }
}

/**
 * A utility function visiting all nodes within a given list of AST root nodes
 * recursively in a depth-first pre-order fashion applying the given function to each
 * encountered node.
 *
 * @param list the list of roots of the ASTs to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename T, typename Lambda>
void visitDepthFirst(const std::vector<std::unique_ptr<T>>& list, const Lambda& fun) {
    for (const auto& cur : list) {
        visitDepthFirst(*cur, fun);
    }
}

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first post-order fashion applying the given function to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename R, typename N>
void visitDepthFirstPostOrder(const AstNode& root, const std::function<R(const N&)>& fun) {
    auto visitor = detail::makeLambdaAstVisitor(fun);
    visitDepthFirstPostOrder<void>(root, visitor);
}

/**
 * A utility function visiting all nodes within the ast rooted by the given node
 * recursively in a depth-first post-order fashion applying the given function to each
 * encountered node.
 *
 * @param root the root of the AST to be visited
 * @param fun the function to be applied
 * @param args a list of extra parameters to be forwarded to the visitor
 */
template <typename Lambda, typename R = typename lambda_traits<Lambda>::result_type,
        typename N = typename lambda_traits<Lambda>::arg0_type>
typename std::enable_if<!detail::is_ast_visitor<Lambda>::value, void>::type visitDepthFirstPostOrder(
        const AstNode& root, const Lambda& fun) {
    visitDepthFirstPostOrder(root, std::function<R(const N&)>(fun));
}

}  // namespace souffle
