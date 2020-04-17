module Parser
  ( Parser (..)
  , string
  , eof
  , integer
  , double
  , word
  , letter
  , digit
  , someSpace
  , space
  , braced
  , between

  , void
  , choice
  , optional
  , followedBy
  , token
  , char
  , notChar
  , oneOf
  , noneOf
  , many
  , some
  , try
  , satisfy

  , failureMessage
  ) where

-- TODO refactor so that parsers have names instead of 'failure messages'
-- TODO have failure message utilize the names and a scheme for reporting them

import Prelude hiding (fail)
import Data.Char (isLetter, isSpace, isNumber)
import Control.Applicative (Alternative (..))
import qualified Control.Monad as M (void)

newtype Parser a =
  Parser
    { runParser :: String -> Either String (a, String)
    }

instance Monad Parser where
  (Parser aParser) >>= aToBParser =
    Parser $ \str -> do
      (aResult, restOfString) <- aParser str
      let (Parser bParser) = aToBParser aResult
      (bResult, restOfString2) <- bParser restOfString
      return (bResult, restOfString2)

instance Alternative Parser where
  empty = 
    Parser $ \str -> 
      Left $ failureMessageOfInput str
  (<|>) = try

instance Applicative Parser where
  pure val = Parser $ \str -> Right (val, str)
  aToBParser <*> aParser =
    Parser $ \str -> do
      (aToBFunc, restOfString1) <- runParser aToBParser str
      runParser (aToBFunc <$> aParser) restOfString1

instance Functor Parser where
  fmap f (Parser parseFunc) =
    Parser $ \str -> do
      (result, restOfString) <- parseFunc str
      return (f result, restOfString)

--
-- PARSER INSTANCES
--

stringLiteral :: Parser String
stringLiteral = do
  char '"'
  value <- many $ notChar '"'
  char '"'
  return value

string :: String -> Parser String
string str = failureMessage ("Expected string: " ++ str) $ make str 
  where 
    make :: String -> Parser String
    make [] = pure []
    make (c:cs) = do
      nextChar <- anyChar 
      restChars <- make cs
      if nextChar /= c
         then empty
         else return $ nextChar : restChars

eof :: Parser ()
eof = try rawParser (fail "Expected end of file")
  where
    rawParser = 
      Parser $ \str ->
        if null str
          then Right ((), str)
          else Left ""

fail :: String -> Parser a
fail message = failureMessage message empty 

integer :: Parser Integer
integer = do
  posNeg <- optional $ oneOf "+-"
  digits <- some digit

  let isNeg = case posNeg of
                Nothing -> False
                Just sym -> sym == '-'
      rawNum = readInteger digits

  return $ if isNeg then (-rawNum) else rawNum

double :: Parser Double
double = do
  characteristic <- fromInteger <$> integer
  _ <- char '.'
  mantissa <- readMantissa <$> many digit
  return $ characteristic + mantissa

readInteger :: String -> Integer
readInteger [] = error "No input"
readInteger cs = foldl (\lastResult c -> (lastResult * 10) + readDigit c) 0 cs

readMantissa :: String -> Double
readMantissa [] = 0
readMantissa (c:cs) = 
  let thisDigitValue = fromInteger $ readDigit c
      restMantissaValue = readMantissa cs
   in (thisDigitValue / 10) + (restMantissaValue / 10)

-- cheating because we *could* just use code point manipulation, 
-- but that's not only trivial (not fun at all) but also outright wrong
readDigit :: Char -> Integer
readDigit c = read [c] 

someSpace :: Parser ()
someSpace = M.void $ many space

space :: Parser Char
space = satisfy isSpace

anyChar :: Parser Char
anyChar = satisfy $ const True

braced :: Parser String
braced = do
  open    <- char '('
  content <- concat <$> many (try notBrace braced) 
  close   <- char ')'
  return $ [open] ++ content ++ [close]
    where 
      notBrace :: Parser String
      notBrace = some $ noneOf "()"

word :: Parser String
word = some $ satisfy (not . isSpace)

digit :: Parser Char
digit = failureMessage "Expected digit" $ satisfy isNumber

letter :: Parser Char
letter = failureMessage "Expected letter" $ satisfy isLetter

--
-- PARSER COMBINATORS
--

between :: Parser a -> Parser b -> Parser c -> Parser c
between bra ket parser = do
  _ <- bra
  res <- parser 
  _ <- ket
  return res

void :: Parser a -> Parser ()
void = (>> pure ())

followedBy :: Parser a -> Parser ()
followedBy parser = Parser $ \str -> 
  (\(value, _) -> ((), str)) <$> runParser parser str

choice :: [Parser a] -> Parser a
choice = foldr try (failureMessage "All choices failed" empty) 

optional :: Parser a -> Parser (Maybe a)
optional parser = Parser $ \str -> 
  case runParser parser str of
    Left msg          -> Right (Nothing, str)
    Right (res, rest) -> Right (Just res, rest)

token :: Parser a -> Parser a
token parser = parser <* someSpace

failureMessage :: String -> Parser a -> Parser a
failureMessage message parser =
  Parser $ \str ->
    overrideLeft (\msg -> message ++ " >> " ++ msg) (runParser parser str)
  where
    overrideLeft :: (a -> a) -> Either a b -> Either a b
    overrideLeft f (Left l) = Left (f l)
    overrideLeft _ right = right

try :: Parser a -> Parser a -> Parser a
try parserA parserB =
  Parser $ \str ->
    case runParser parserA str of
      Left _ -> runParser parserB str
      right -> right

-- 
-- PARSER CONSTRUCTORS 
--

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser parseFunc
  where
    parseFunc [] = Left "Exhausted input"
    parseFunc (c:cs) =
      if predicate c
        then Right (c, cs)
        else Left $ failureMessageOfInput (c:cs)

char :: Char -> Parser Char
char char =
  failureMessage ("Expected char [" ++ [char] ++ "]") $ satisfy (char ==)

oneOf :: String -> Parser Char
oneOf cs =
  failureMessage message $ satisfy (`elem` cs)
    where 
      message = "Expected one of [" ++ cs ++ "]"

noneOf :: String -> Parser Char
noneOf cs = 
  failureMessage message $ satisfy (`notElem` cs)
    where 
      message = "Expected none of [" ++ cs ++ "]"

notChar :: Char -> Parser Char
notChar char = 
  failureMessage ("Expected not char [" ++ [char] ++ "]") $ satisfy (char /=)

--
-- MISC
--

failureMessageOfInput [] = "Failed at end of input"
failureMessageOfInput [c] = 
  "Failed at char '" ++ [c] ++ "' at end of input"
failureMessageOfInput (c:cs) = 
  "Failed at char '" ++ [c] ++ "' in: '" ++ (c : take 10 cs) ++ "'"

