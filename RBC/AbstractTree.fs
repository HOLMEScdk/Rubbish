﻿module RBC.AbstractTree

type Program = Declaration list

and Declaration =
    | StaticVariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and TypeSpec =
    | Void
    | Bool
    | Int
    | Float
    | Char

and VariableDeclaration =                          (* void/int/../ define var and arr *)
    | ScalarVariableDeclaration of TypeSpec * Identifier
    | ArrayVariableDeclaration of TypeSpec * Identifier

and FunctionDeclaration = TypeSpec * Identifier * Parameters * CompoundStatement

and Identifier = string

and Parameters = VariableDeclaration list

and IdentifierRef = { Identifier : string; }     (* Ref record identifer*)

and Statement =
    | ExpressionStatement of ExpressionStatement
    | CompoundStatement of CompoundStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of Expression option
    | BreakStatement

and ExpressionStatement =
    | Expression of Expression
    | Nop

and CompoundStatement = LocalDeclarations * Statement list

and LocalDeclarations = VariableDeclaration list

and IfStatement = Expression * Statement * Statement option

and WhileStatement = Expression * Statement

and Expression =
    | ScalarAssignmentExpression of IdentifierRef * Expression
    | ArrayAssignmentExpression of IdentifierRef * Expression * Expression
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | IdentifierExpression of IdentifierRef
    | ArrayIdentifierExpression of IdentifierRef * Expression
    | FunctionCallExpression of Identifier * Arguments
    | ArraySizeExpression of IdentifierRef
    | LiteralExpression of Literal
    | ArrayAllocationExpression of TypeSpec * Expression

and BinaryOperator =
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreaterEqual
    | Greater
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus

and UnaryOperator =
    | LogicalNegate
    | Negate
    | Identity

and Arguments = Expression list

and Literal =
    | BoolLiteral of bool
    | IntLiteral of int
    | FloatLiteral of float
    | CharLiteral of string