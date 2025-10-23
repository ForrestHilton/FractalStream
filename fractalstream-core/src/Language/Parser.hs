module Language.Parser
  (
   tokenize
  , tokenizeWithIndentation
  , Token(..)
  , SRToken
  , SourceRange(..)
  , SourceSpan(..)
  , spanOfSourceRange

  , TC(..)
  , Expected(..)
  , ParseError(..)
  , TCError(..)
  , An(..)
  , tryEach
  , internal
  , HasErrorLocation(..)
  , ppError
  , PrettyPrint(..)
  , ppFullError
  , advise

  , optional
  , void
  , (<|>)
  , empty
  , many
  , some
  , (<&>)
  , ($>)
  , (<$)

  -- * Functions that behave as the like-named ones
  -- from Text.Earley, except also track source ranges
  , token
  , tokenMatch
  , satisfy
  , ruleChoice
  , rule
  , Prod(..)
  , withSourceRange
  , (<?>)
  , parse

   -- * Re-exports from Text.Earley
  , P.Grammar

  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Ord
import qualified Text.Earley as P
import Language.Type

------------------------------------------------------
-- Tokenization
------------------------------------------------------

identChar :: Char -> Bool
identChar c = (isAlphaNum c && not (c `elem` superscripts) && (c /= '⁻'))
              || c == '_'

opTokens :: [(String, Token)]
opTokens = sortOn (\x -> (Down (length (fst x)), x)) $
  [ ("+", Plus), ("-", Minus), ("*", Times)
  , ("//", IntegerDivide), ("/", Divide)
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

longestMatchingOperator :: SRString -> Maybe (SRToken, SRString)
longestMatchingOperator cs =
  listToMaybe [ ( SRToken t (mconcat $ map snd $ take (length s) cs)
                , drop (length s) cs)
              | (s, t) <- opTokens, s `isPrefixOf` (map fst cs) ]

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
  = Group [SRToken] [TokenGroup]
  | Single [SRToken]
  deriving (Eq, Show)

toTok :: TokenGroup -> [SRToken]
toTok (Single s) = s ++ [SRToken Newline NoSourceRange]
toTok (Group s xs) = [SRToken Indent NoSourceRange] ++ s ++
                     [SRToken Newline NoSourceRange] ++
                     concatMap toTok xs ++ [SRToken Dedent NoSourceRange]

tokenize :: String -> [SRToken]
tokenize = tokenize' .
  zipWith (\i c -> let p = Pos 0 i in (c, SourceRange p p)) [0..]

tokenize' :: SRString -> [SRToken]
tokenize' = \case
  -- Done!
  [] -> []

  -- Skip whitespace
  (c:cs) | isSpace (fst c) -> tokenize' cs

  -- Skip comments
  (('#',_):_) -> []

  -- Tokenize positive numbers
  cs@(d:_)
    | isDigit (fst d) ->
        let ds = takeWhile (isDigit . fst) cs
        in case dropWhile (isDigit . fst) cs of
          (('.',sr) : cs'@(d' : _))
            | isDigit (fst d') ->
                let ds' = ds <> [('.', sr)] <> takeWhile (isDigit . fst) cs'
                in SRToken (NumberF (read $ map fst ds')) (mconcat $ map snd ds')
                   : tokenize' (dropWhile (isDigit . fst) cs')
          cs' -> SRToken (NumberI (read $ map fst ds)) (mconcat $ map snd ds)
                 : tokenize' cs'

  -- Tokenize the ⁻, ⁰, ¹, ², ³, ⁴, ⁵, ⁶, ⁷, ⁸, ⁹ superscripts
  (('⁻',sr):cs@(d:_))
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in SRToken Caret NoSourceRange
            : SRToken (NumberI (negate $ superscriptNumber (map fst ds)))
                    (sr <> mconcat (map snd ds))
            : tokenize' cs'
  cs@(d:_)
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in SRToken Caret NoSourceRange
            : SRToken (NumberI (superscriptNumber (map fst ds)))
                    (mconcat (map snd ds))
            : tokenize' cs'

  -- Tokenize special operators
  cs | Just (tok, cs') <- longestMatchingOperator cs
       -> tok : tokenize' cs'

  -- Tokenize identifiers
  cs@(c:_) | isAlpha (fst c) && not (fst c `elem` superscripts) && (fst c /= '⁻') ->
               let ds = takeWhile (identChar . fst) cs
                   cs' = dropWhile (identChar . fst) cs
                   tok = case Map.lookup (map fst ds) wordlikeTokens of
                     Just t  -> t
                     Nothing -> Identifier (map fst ds)
               in SRToken tok (mconcat (map snd ds)) : tokenize' cs'

  -- Otherwise, grab a junk character
  ((c, sr):cs) -> SRToken (Junk c) sr : tokenize' cs

superscripts :: String
superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹"

superscriptNumber :: String -> Integer
superscriptNumber = go 0
  where
    go acc = let next i = go (10 * acc + i)
             in \case
      [] -> acc
      ('⁰':xs) -> next 0 xs
      ('¹':xs) -> next 1 xs
      ('²':xs) -> next 2 xs
      ('³':xs) -> next 3 xs
      ('⁴':xs) -> next 4 xs
      ('⁵':xs) -> next 5 xs
      ('⁶':xs) -> next 6 xs
      ('⁷':xs) -> next 7 xs
      ('⁸':xs) -> next 8 xs
      ('⁹':xs) -> next 9 xs
      _ -> error "should be impossible"

type SRString = [(Char, SourceRange)]

tokenizeWithIndentation :: String -> [SRToken]
tokenizeWithIndentation
         = ([SRToken Indent NoSourceRange] ++)
         . (++ [SRToken Dedent NoSourceRange])
         . concatMap toTok
         . block
         . map observeSpaces
         . filter (not . all (isSpace . fst))
         . map dropComments
         . map (\(r, xs) -> map (\(c, x) -> let p = Pos r c
                                            in (x, SourceRange p p)) xs)
         . zip [0..]
         . map (zip [0..])
         . lines
  where
    dropComments :: SRString -> SRString
    dropComments = \case
      []      -> []
      (('#',_):_) -> []
      (c:cs)  -> c : dropComments cs

    observeSpaces :: SRString -> (Int, SRString)
    observeSpaces s = (length (takeWhile (isSpace . fst) s)
                      , dropWhile (isSpace . fst) s)

    -- Dedent all lines by n spaces
    extract :: Int -> [(Int, SRString)] -> ([TokenGroup], [(Int, SRString)])
    extract n ss =
      let ss'  = takeWhile ((>= n) . fst) ss
          ss'' = dropWhile ((>= n) . fst) ss
      in (block [(m - n, x) | (m,x) <- ss'], ss'')

    block :: [(Int, SRString)] -> [TokenGroup]
    block = \case
      [] -> []
      ((n,s) : ss)
        | n == 0 -> Single (tokenize' s) : block ss
        | n > 0 ->
            let (is, ss') = extract n ss
            in Group (tokenize' s) is : block ss'
      _ -> error "bad indent"

data Token
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | IntegerDivide
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

------------------------------------------------------
-- Source ranges
------------------------------------------------------

data SRToken = SRToken
  { baseToken :: Token
  , tokenSourceRange :: SourceRange
  }
  deriving (Show)

-- | CAUTION! Sketchy Eq instance ahead!
instance Eq SRToken where
  t1 == t2 = baseToken t1 == baseToken t2

data SourceRange
  = SourceRange Pos Pos
  | NoSourceRange
  deriving (Eq, Show)

data Pos = Pos { posRow :: Int, posCol :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup SourceRange where
  NoSourceRange <> r = r
  r <> NoSourceRange = r
  SourceRange s e <> SourceRange s' e' =
    SourceRange (min s s') (max e e')

instance Monoid SourceRange where
  mempty = NoSourceRange

------------------------------------------------------
-- Versions of types and combinators from Text.Earley,
-- modified so that they also track source ranges.
------------------------------------------------------

newtype Prod r a = Prod { runProd :: P.Prod r String SRToken (SourceRange, a) }
  deriving Functor

instance Applicative (Prod r) where
  pure = Prod . pure . (NoSourceRange,)
  Prod mf <*> Prod ma = Prod $
    (\(sr, f) (sr', x) -> (sr <> sr', f x)) <$> mf <*> ma

instance Alternative (Prod r) where
  empty = Prod empty
  Prod x <|> Prod y = Prod (x <|> y)
  many (Prod p) = Prod (sequence <$> many p)
  some (Prod p) = Prod (sequence <$> some p)

instance Semigroup a => Semigroup (Prod r a) where
  Prod px <> Prod py = Prod $
    (\(sr,x) (sr',y) -> (sr <> sr', x <> y)) <$> px <*> py

instance Monoid a => Monoid (Prod r a) where
  mempty = Prod (pure (NoSourceRange, mempty))

token :: Token -> Prod r Token
token bt = satisfy (== bt)

tokenMatch :: (Token -> Maybe a) -> Prod r a
tokenMatch f = satisfy (isJust . f) <&> \t -> case f t of
    Just x  -> x
    Nothing -> error "impossible"

satisfy :: (Token -> Bool) -> Prod r Token
satisfy f = Prod ((\(SRToken bt sr) -> (sr, bt)) <$> P.satisfy (f . baseToken))

ruleChoice :: [Prod r a] -> P.Grammar r (Prod r a)
ruleChoice = rule . asum

rule :: Prod r a -> P.Grammar r (Prod r a)
rule = fmap Prod . P.rule . runProd

-- | A named production (used for reporting expected things).
infixr 0 <?>
(<?>) :: Prod r a -> String -> Prod r a
Prod p <?> name = Prod (p P.<?> name)

parse :: (forall r. P.Grammar r (Prod r a)) -> [SRToken] -> Either ParseError a
parse p input = case P.fullParses (P.parser (fmap snd . runProd <$> p)) input of
    ([], r)  -> Left (NoParse `onReport` r)
    ([x], _) -> pure x
    (_, r)   -> Left (AmbiguousParse `onReport` r)
  where
    onReport f P.Report{..} = f (toPos position) (Set.fromList expected)
    toPos i = case mconcat (take i (map tokenSourceRange input)) of
      NoSourceRange   -> Nothing
      SourceRange _ e -> Just e

withSourceRange :: Prod r (SourceRange -> a) -> Prod r a
withSourceRange (Prod p) = Prod $ (\(sr, f) -> (sr, f sr)) <$> p

data SourceSpan
  = InLine Int Int Int
  | InRows Int Int

spanOfSourceRange :: SourceRange -> Maybe SourceSpan
spanOfSourceRange = \case
  NoSourceRange -> Nothing
  SourceRange (Pos r c) (Pos r' c')
    | r == r'   -> Just (InLine r c c')
    | otherwise -> Just (InRows r r')

----------------------------------------------------
-- A monad for the type- and environment-checking
-- stage after the immediate parse.
----------------------------------------------------

newtype TC a = TC (Either TCError a)
  deriving (Show, Functor, Applicative, Monad, MonadError TCError)

newtype Expected t = Expected t
  deriving Show

data ParseError
  = NoParse (Maybe Pos) (Set String)
  | AmbiguousParse (Maybe Pos) (Set String)
  deriving Show

data TCError
  = Surprise SourceRange String String (Expected String)
  | MissingName SourceRange String
  | AlreadyDefined SourceRange String
  | BadConversion SourceRange SomeType (Expected SomeType)
  | Advice SourceRange String
  | Internal TCError
  deriving Show

internal :: TCError -> TCError
internal e = case e of
  Internal _ -> e
  _ -> Internal e

tryEach :: TCError -> [TC a] -> TC a
tryEach failure = go
  where
    go = \case
      [] -> throwError failure
      (x:xs) -> catchError x (\_ -> go xs)

class HasErrorLocation a where
  errorLocation :: a -> SourceRange

instance HasErrorLocation TCError where
  errorLocation = \case
    Surprise sr _ _ _    -> sr
    MissingName sr _     -> sr
    AlreadyDefined sr _  -> sr
    BadConversion sr _ _ -> sr
    Advice sr _          -> sr
    Internal e -> errorLocation e

instance HasErrorLocation ParseError where
  errorLocation = \case
    NoParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    AmbiguousParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp

instance (HasErrorLocation a, HasErrorLocation b) => HasErrorLocation (Either a b) where
  errorLocation = either errorLocation errorLocation

class An a where
  an :: a -> String

instance An String where
  an x = case x of
    "" -> "???"
    (c:_)
      | c `elem` "aeiouAEIOU" -> "an " ++ x
      | otherwise -> "a " ++ x

instance An SomeType where
  an = an . show

instance KnownType t => An (TypeProxy t) where
  an = an . SomeType

class PrettyPrint a where
  pp :: a -> [String]

expectingClause :: Set String -> String
expectingClause prods = case unsnoc (Set.toList prods) of
  Nothing  -> "."
  Just ([], x) -> ", where I was expecting " ++ x ++ "."
  Just ([x'], x) -> ", where I was expecting either " ++ x' ++ " or " ++ x ++ "."
  Just (xs, x)   -> ", where I was expecting one of " ++ intercalate ", " xs ++ ", or " ++ x ++ "."

instance PrettyPrint ParseError where
  pp = (:[]) . \case
    NoParse _ prods -> "I got stuck and couldn't parse the input here" ++ expectingClause prods
    AmbiguousParse _ prods -> "I got stuck because the grammar is ambiguous here" ++ expectingClause prods

instance PrettyPrint TCError where
  pp = (:[]) . ppError

ppError :: TCError -> String
ppError = concat .  \case
  Surprise _ thing got (Expected wanted) ->
    ["I expected ", wanted, " here, but ", thing, " is ", got, "."]
  MissingName _ name ->
    ["No variable named ", name, " is defined here."]
  AlreadyDefined _ name ->
    ["The variable ", name, " is already defined, and would be re-defined here."]
  BadConversion _ ty (Expected ety) ->
    ["Conversion to ", an ty, " was used here, but ", an ety, " was expected."]
  Advice _ advice -> [advice]
  Internal e ->
    ["INTERNAL ERROR, please report at ",
      "https://github.com/matt-noonan/FractalStream/issues: ",
      ppError e]

instance PrettyPrint (SourceRange, String) where
  pp (sr, input) =
    let inputLines = lines input
    in case spanOfSourceRange sr of
         Nothing -> []
         Just (InLine i s e) -> case drop (i - 1) inputLines of
           [] -> []
           (line:_) -> map ("  " ++) [line, replicate s ' ' ++ replicate (e + 1 - s) '^']
         Just (InRows s e) -> map ("| " ++) . take (e + 1 - s) . drop (s - 1) $ inputLines

advise :: SourceRange -> String -> TC a
advise sr = throwError . Advice sr

ppFullError :: Either ParseError TCError -> String -> String
ppFullError err input =
  let msg = either pp pp err
      context = pp (errorLocation err, input)
  in unlines (context ++ msg)
