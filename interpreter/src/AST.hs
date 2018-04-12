{-# LANGUAGE GADTs #-}

module AST where

type VariableName = String

data Expression =
    Block Expression
  | Chain [Expression]
  | Lambda VariableName Expression
  | Lazy Expression
  | LazyEvaluate Expression
  | Call Expression Expression
  | If Expression Expression Expression
  | Declaration VariableName Expression
  | Variable VariableName
  | Nonlocal VariableName
  | EraseVar VariableName

 -- Literals
  | UnitLiteral
  | BoolLiteral Bool
  | IntLiteral Integer
  | StringLiteral String
  | AssignmentLiteral VariableName
  | OperatorLiteral BuiltInOperator
  deriving (Eq, Show)


data BuiltInOperator =
    Plus
  | Minus
  | Mult
  | Pow
  | Concat
  | Equal
  | Less
  | Print
  | Debug
  deriving (Eq, Show)
