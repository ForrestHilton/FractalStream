{-# language OverloadedStrings #-}
{-# options_ghc -Wno-x-partial #-}
module Language.Parser
  (
   tokenize
  , tokenizeWithIndentation
  , Token(..)
  , Errs(..)
  , optional
  , void
  , (<|>)
  , empty
  , many
  , some
  , (<&>)
  , ($>)
  , (<$)

   -- * Re-exports from Text.Earley
  , Grammar
  , Prod
  , rule
  , token
  , satisfy
  , (<?>)
  , fullParses
  , parser

  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import Data.Char
import Data.List (isPrefixOf, sortOn)
import Data.Ord
import Text.Earley

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'

opTokens :: [(String, Token)]
opTokens = sortOn (\x -> (Down (length (fst x)), x)) $
  [ ("+", Plus), ("-", Minus), ("*", Times), ("/", Divide)
  , ("^", Caret), ("|", Bar), ("(", OpenParen), (")", CloseParen)
  , ("[", OpenBracket), ("]", CloseBracket)
  , ("{", OpenBrace), ("}", CloseBrace)
  , (",", Comma), (":", Colon), (";", Semicolon)
  , (">", GreaterThan), ("<", LessThan)
  , ("==", Equal), ("=", Equal)
  , (">=", GreaterThanOrEqual), ("<=", LessThanOrEqual)
  , ("≥", GreaterThanOrEqual), ("≤", LessThanOrEqual)
  , ("!=", NotEqual), ("=/=", NotEqual), ("≠", NotEqual)
  , ("->", RightArrow), ("<-", LeftArrow)
  , ("⭢", RightArrow), ("⭠", LeftArrow)
  , ("→", RightArrow), ("←", LeftArrow)
  ]

longestMatchingOperator :: String -> Maybe (Token, String)
longestMatchingOperator cs =
  listToMaybe [ (t, drop (length s) cs)
              | (s, t) <- opTokens, s `isPrefixOf` cs ]

wordlikeTokens :: Map String Token
wordlikeTokens = Map.fromList
  [ ("if", If), ("then", Then), ("else", Else)
  , ("e", Euler), ("𝑒", Euler)
  , ("pi", Pi), ("π", Pi)
  , ("i", I), ("𝑖", I)
  , ("true", True_), ("false", False_)
  , ("or", Or_), ("and", And_), ("not", Not_)
  ]
data TokenGroup
  = Group [Token] [TokenGroup]
  | Single [Token]
  deriving (Eq, Ord, Show)

toTok :: TokenGroup -> [Token]
toTok (Single s) = s ++ [Newline]
toTok (Group s xs) = [Indent] ++ s ++ [Newline] ++ concatMap toTok xs ++ [Dedent]

tokenize :: String -> [Token]
tokenize = \case
  -- Done!
  [] -> []

  -- Skip whitespace
  (c:cs) | isSpace c -> tokenize cs

  -- Tokenize negative numbers
  ('-':cs@(d:_))
    | isDigit d ->
      let ds = '-' : takeWhile isDigit cs
      in case dropWhile isDigit cs of
            ('.' : cs'@(d' : _))
              | isDigit d' ->
                  let ds' = ds <> "." <> takeWhile isDigit cs'
                  in NumberF (read ds') : tokenize (dropWhile isDigit cs')
            cs' -> NumberI (read ds) : tokenize cs'

  -- Tokenize positive numbers
  cs@(d:_)
    | isDigit d ->
        let ds = takeWhile isDigit cs
        in case dropWhile isDigit cs of
          ('.' : cs'@(d' : _))
            | isDigit d' ->
                let ds' = ds <> "." <> takeWhile isDigit cs'
                    in NumberF (read ds') : tokenize (dropWhile isDigit cs')
          cs' -> NumberI (read ds) : tokenize cs'

  -- Tokenize special operators
  cs | Just (tok, cs') <- longestMatchingOperator cs
       -> tok : tokenize cs'

  -- Tokenize identifiers
  cs@(c:_) | isAlpha c ->
               let ds = takeWhile ident cs
                   cs' = dropWhile ident cs
                   tok = case Map.lookup ds wordlikeTokens of
                     Just t  -> t
                     Nothing -> Identifier ds
               in tok : tokenize cs'

  -- Otherwise, grab a junk character
  (c:cs) -> Junk c : tokenize cs

tokenizeWithIndentation :: String -> [Token]
tokenizeWithIndentation
         = ([Indent] ++) . (++ [Dedent])
         . concatMap toTok
         . block
         . map observeSpaces
         . filter (not . all isSpace)
         . lines
  where
    observeSpaces :: String -> (Int, String)
    observeSpaces s = (length (takeWhile isSpace s), dropWhile isSpace s)

    -- Dedent all lines by n spaces
    extract :: Int -> [(Int, String)] -> ([TokenGroup], [(Int, String)])
    extract n ss =
      let ss'  = takeWhile ((>= n) . fst) ss
          ss'' = dropWhile ((>= n) . fst) ss
      in (block [(m - n, x) | (m,x) <- ss'], ss'')

    block :: [(Int, String)] -> [TokenGroup]
    block = \case
      [] -> []
      ((n,s) : ss)
        | n == 0 -> Single (tokenize s) : block ss
        | n > 0 ->
            let (is, ss') = extract n ss
            in Group (tokenize s) is : block ss'
      _ -> error "bad indent"

data Token
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | Caret
  | Bar
  | Identifier String
  | Junk Char
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Colon
  | Semicolon
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  | Indent
  | Dedent
  | Newline
  | If
  | Then
  | Else
  | Euler
  | Pi
  | I
  | True_
  | False_
  | Or_
  | And_
  | Not_
  | LeftArrow
  | RightArrow
  deriving (Eq, Ord, Show)

instance IsString Token where fromString = Identifier

newtype Errs a = Errs (Either [String] a)
  deriving (Show, Functor, Applicative, Monad)

instance Alternative Errs where
  Errs xs <|> Errs ys = Errs $ case (xs, ys) of
    (Right v, _) -> Right v
    (_, Right v) -> Right v
    (Left xer, Left yer) -> Left (xer ++ yer)
  empty = Errs (Left [])
