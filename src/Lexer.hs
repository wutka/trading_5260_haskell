module Lexer where

import Data.String
import Data.Char
import Data.List

data Token
  = TokenTransform
  | TokenTransfer
  | TokenSymbol String
  | TokenString String
  | TokenInt Int
  | TokenDouble Double
  | TokenInputs
  | TokenOutputs
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenStar
  | TokenSlash
  | TokenPlus
  | TokenMinus
  | TokenEqual
  | TokenNotEqual
  | TokenGreater
  | TokenGreaterEqual
  | TokenLess
  | TokenLessEqual
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenComputedField
  | TokenUpdatedField
  | TokenThreshold
  deriving (Show, Eq)

parseError :: [Token] -> a
parseError t = error ("Parse Error: " ++ show t)

getTokenSymbol :: Token -> String
getTokenSymbol (TokenSymbol s) = s
getTokenSymbol t = error ("Tried to get symbol from non-TokenSymbol "++show t)

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c == '_' || c == '?'

isSymbolChar :: Char -> Bool
isSymbolChar c = isSymbolStart c || isDigit c

isValidSymbol :: String -> Bool
isValidSymbol s = isSymbolStart (head s) && all isSymbolChar (tail s)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isSymbolStart c = lexSymbol (c:cs)
      | isDigit c = lexInt (c:cs)
      | c == '-' = lexInt (c:cs)
      | c == '"' = lexString cs []
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('*':cs) = TokenStar : lexer cs
lexer ('/':cs) = TokenSlash : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('>':'=':cs) = TokenGreaterEqual : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs
lexer ('<':'=':cs) = TokenLessEqual : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('=':cs) = TokenEqual : lexer cs
lexer ('!':'=':cs) = TokenNotEqual : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs

lexer cs = error ("Unexpected token at "++cs)

isNumChar ch = isDigit ch || ch == '-' || ch == '.'

lexInt :: String -> [Token]
lexInt s =
  if isMinus then
    TokenMinus : lexer restChars
  else if isDouble then
    TokenDouble doubleValue : lexer restChars
  else
    TokenInt intValue : lexer restChars
  where
    digits = takeWhile isNumChar s
    isDouble = '.' `elem` digits
    restChars = dropWhile isNumChar s
    intValue = read digits
    doubleValue = read digits
    isMinus = digits == "-"

lexSymbol :: String -> [Token]
lexSymbol s =
  if lowerSymChars == "transform" then
    TokenTransform : lexer restChars
  else if lowerSymChars == "transfer" then
    TokenTransfer : lexer restChars
  else if lowerSymChars == "inputs" then
    TokenInputs : lexer restChars
  else if lowerSymChars == "outputs" then
    TokenOutputs : lexer restChars
  else if lowerSymChars == "computedfield" then
    TokenComputedField : lexer restChars
  else if lowerSymChars == "updatedfield" then
    TokenUpdatedField : lexer restChars
  else if lowerSymChars == "threshold" then
    TokenThreshold : lexer restChars
  else
    TokenSymbol symChars : lexer restChars
  where
    symChars = takeWhile isSymbolChar s
    lowerSymChars = map toLower symChars
    restChars = dropWhile isSymbolChar s
    
    
lexString :: String -> String -> [Token]
lexString [] acc = [TokenString $ reverse acc]
lexString ('"':'"':l) acc = lexString l ('"':acc)
lexString ('"':l) acc = TokenString (reverse acc) : lexer l
lexString (lc:l) acc = lexString l (lc:acc)
