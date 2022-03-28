module Parser (
  module Text.Megaparsec,

  Parser,
  parseRepl,
  parseProgram
)
where

import Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Types

type Parser = Parsec Void String

comment :: Parser ()
comment = hidden $ Lex.skipLineComment ";"

{-
  The order of the parsers in choice here is pretty important.
  
  For example: 
    nil, true, false need to be tried before Sym since
    they can also be parsed as valid symbols

  Another example is 
    132

  which can be completely parsed by parseInt, or partially parsed
  by parseFloat (ending in failure). However in both cases, the
  parsers consume > 0 chars, which blocks choice (As choice picks
  the first parser to consume a non zero number of chars, even if 
  they fail)
-}

parseAtom :: Parser Expr
parseAtom = choice [
    ENil <$ string "nil",
    EBool True <$ string "true",
    EBool False <$ string "false",
    EChar <$> (char '#' >> Lex.charLiteral),
    EStr <$> parseStr,
    EKey <$> (char ':' >> parseSymbol) <?> "keyword",
    try parseFloat,
    try parseInt,
    ESym <$> parseSymbol <?> "symbol"
  ] <?> "atom"
  where
    parseSymbol = some (noneOf " \"\t\r\n;#()[]{}")

    parseFloat = EFloat <$> Lex.signed (pure ()) Lex.float
    parseInt   = EInt <$> Lex.signed (pure ()) Lex.decimal

    parseStr = do
      char '"'
      >> manyTill Lex.charLiteral (char '"' <?> "closing quote of string")

parseExpr :: Parser Expr
parseExpr = choice [
    {-
      All three brackets are completely equivalent.

      The only reason I added them was to make it slightly
      easier for me to read S-expressions, since it breaks up
      the uniformness of the code.
    -}
    EList <$> parseBlock "(" ")",
    EList <$> parseBlock "[" "]",
    EList <$> parseBlock "{" "}",

    {- Standard syntax sugar for meta quoting operators -}
    "'"  <: "quote",
    "`"  <: "quasiquote",
    try $ "~@" <: "splice-unquote",
    "~"  <: "unquote",

    parseAtom
  ] <?> "expression"
  where
    parseBlock a b = string a
      >> space
      >> parseExpr `sepEndBy` space1
      <* string b

    desugar name parser = (\x -> EList [ESym name, x]) <$> parser
    prefix <: name = desugar name (string prefix >> parseExpr)

parseProgram :: Parser [Expr]
parseProgram = do 
  space 
  many (parseExpr <* space) <* eof

parseRepl :: Parser (Maybe Expr)
parseRepl = do
  -- This is the only place i've managed to get comments to work

  val <- space >> optional parseExpr 
  space >> optional comment >> eof
  pure val