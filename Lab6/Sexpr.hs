--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String  
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"
  
parseFloatHelper :: Parser String 
parseFloatHelper = do 
  e <- oneOf "eE"
  sign2 <- option "" (string "-" <|> string "+")
  exp <- many1 digit
  return ([e] ++ sign2 ++ exp)

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  exponent <- option "" parseFloatHelper
  return (read (sign ++ digits ++ "." ++ f ++ exponent) :: Double)
  <?> "floating-point number"
  
parseString :: Parser String
parseString = do 
  char '\"'
  str <- many (noneOf "\"")
  char '\"'
  return str

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"
  
parseHelper :: Char -> Char -> Parser [Sexpr]
parseHelper x y = do 
  char x
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char y
  return ss
  <?> "Helper function to parse lists"

-- C.3 
{-
This function does not use the try combinator because it should never 
have to backtrack. We are using alternation, because the first character
has a specific closing bracket, and we will never enter a parser and 
then have to backtrack, because they are distinct. If one parser is 
entered, then the others are guaranteed not to be!
-}

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = 
  parseHelper '(' ')'
  <|> parseHelper '[' ']'
  <|> parseHelper '{' '}'
  <?> "list of S-expressions"

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = do 
  char '\'' 
  expr <- parseSexpr
  return(ListS[AtomS (IdA "quote"), expr])
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"


-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

