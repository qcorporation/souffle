// -- arguments --
GO(IntrinsicFunctor, Functor)
GO(UserDefinedFunctor, Functor)

GO(NilConstant, Constant)
GO(NumericConstant, Constant)
GO(StringConstant, Constant)

GO(Functor, Term)
GO(RecordInit, Term)

GO(Aggregator, Argument)
GO(Constant, Argument)
GO(Counter, Argument)
GO(SubroutineArgument, Argument)
GO(Term, Argument)
GO(TypeCast, Argument)
GO(UnnamedVariable, Argument)
GO(Variable, Argument)

GO(Argument, Node)

// literals
GO(BinaryConstraint, Constraint)
GO(BooleanConstraint, Constraint)

GO(ProvenanceNegation, Negation)

GO(Atom, Literal)
GO(Body, Literal)
GO(Constraint, Literal)
GO(Negation, Literal)

GO(Literal, Node)

// -- types --
GO(SubsetType, Type)
GO(RecordType, Type)
GO(UnionType, Type)
GO(Type, Node)

// components
GO(Component, Node)
GO(ComponentInit, Node)
GO(ComponentType, Node)

// -- others --
GO(Attribute, Node)
GO(Clause, Node)
GO(ExecutionOrder, Node)
GO(ExecutionPlan, Node)
GO(IO, Node)
GO(Pragma, Node)
GO(Program, Node)
GO(Relation, Node)
