{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import AST

import Data.Char (toLower)
import Data.List (foldl1')
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Token (TokenParser, GenTokenParser(TokenParser), makeTokenParser,
                          LanguageDef, GenLanguageDef(..))

TokenParser{
    T.identifier, T.reserved,
    T.operator, T.reservedOp,
    T.charLiteral, T.stringLiteral = tokenStringLiteral,
    T.natural, T.integer, T.float, T.naturalOrFloat, T.decimal, T.hexadecimal, T.octal,
    T.symbol, T.lexeme, T.whiteSpace,
    T.parens, T.braces, T.angles, T.brackets,
    T.semi, T.comma, T.colon, T.dot,
    T.semiSep, T.semiSep1, T.commaSep, T.commaSep1
    } =
  makeTokenParser $ LanguageDef {
    commentStart = "{-",
    commentEnd = "-}",
    commentLine = "--",
    nestedComments = True,
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_' <|> char '-',
    -- opStart  = oneOf "!#$%&*+<=@\\^|-~",
    opStart  = oneOf ":!#$%&*+./<=>?@\\^|-~",
    opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    reservedNames = [],
    reservedOpNames = [],
    caseSensitive = False
    } :: TokenParser ()


skip :: Monad m => m a -> m b -> m a
skip a b = do x <- a
              b
              return x

parseString :: String -> Either ParseError Expression
parseString = parse program ""

parseFile :: FilePath -> IO (Either ParseError Expression)
parseFile = (parseString <$>) . readFile


program, expression, term, block, chain, lazy, lazyTerm, lazyEvaluate,
    lambda, callChain,
    _if, declaration, variable,
    unit, boolLiteral, intLiteral, stringLiteral,
    assignmentLiteral, operatorLiteral,
    upToLambda
    :: Parsec String () Expression

program = whiteSpace >> expression `skip` eof

expression = choice [
    try chain,
    try upToLambda
    ] `skip` optional dot

upToLambda = choice [
    try lambda,
    try lazy,
    try declaration,
    try _if,
    try callChain
    ]

-- Anything that can be the argument to a function without extra brackets
term = choice [try unit, try boolLiteral, try intLiteral, stringLiteral,
               try lazyEvaluate,
               try lazyTerm,
               try assignmentLiteral, try operatorLiteral,
               try nonlocal, try eraseVar,
               variable, block]

block = Block <$>
    (parens expression <|> brackets expression <|> braces expression)

chain = Chain <$> sepEndBy1 upToLambda (dot <|> comma)

lambda = do
    args <- many1 $ optional (symbol "∙" <|> symbol "·") >> variableName
    symbol "→" <|> symbol "->"
    expr <- upToLambda
    return (foldr Lambda expr (reverse args))

lazy = symbol "!" >> (symbol "→" <|> symbol "->") >> Lazy <$> upToLambda

lazyTerm = Lazy . Block <$> angles expression

lazyEvaluate = LazyEvaluate <$> (symbol "!" >> term)

-- Zero, one or multiple calls with precedence x y z f = x (y (z f))
-- https://stuckinaninfiniteloop.blogspot.se/2011/10/left-recursion-in-parsec.html
callChain = term `chainr1` return Call

_if = do cond <- callChain
         symbol "✓" <|> symbol "?"
         true <- callChain
         false <- ((symbol "✗" <|> symbol "/") >> upToLambda)
                  <|> return UnitLiteral
         return (If cond true false)

declaration = do name <- variableName
                 symbol ":"
                 expr <- upToLambda
                 return (Declaration name expr)

variableName = map toLower <$> identifier

variable = Variable <$> variableName

unit = reserved "UNIT" >> return UnitLiteral

boolLiteral = BoolLiteral <$> ((reserved "TRUE" >> return True)
                           <|> (reserved "FALSE" >> return False))

intLiteral = IntLiteral <$> integer

stringLiteral = StringLiteral <$> tokenStringLiteral

assignmentLiteral = AssignmentLiteral <$> variableName `skip` (symbol "←" <|> symbol "<-")

nonlocal = Nonlocal <$> variableName `skip` (symbol "@")

eraseVar = EraseVar <$> variableName `skip` (symbol "~")

operatorLiteral = OperatorLiteral <$> choice [
    try (symbol "++" >> return Concat),
    symbol "+" >> return Plus,
    symbol "-" >> return Minus,
    symbol "*" >> return Mult,
    symbol "^" >> return Pow,
    symbol "=" >> return Equal,
    symbol "<" >> return Less,
    reserved "print" >> return Print,
    reserved "debug" >> return Debug]
