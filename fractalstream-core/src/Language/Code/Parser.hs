{-# language RecursiveDo, DataKinds #-}
module Language.Code.Parser
  ( parseCode
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser hiding (many)
import Language.Value
import Language.Value.Parser
import Language.Code
import Language.Draw

newtype EnvCode = EnvCode
  (forall env. EnvironmentProxy env -> TC (Code env))

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env splices input = do
  EnvCode c <- first Left (parse (codeGrammar splices) (tokenizeWithIndentation input))
  case c env of TC x -> first Right x

ident :: Prod r String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing

tok :: String -> Prod r ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  _ -> Nothing

codeGrammar :: forall r
             . Splices
            -> Grammar r (Prod r EnvCode)
codeGrammar splices = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  block <- ruleChoice
    [ withSourceRange (mkBlock <$> (token Indent *> many toplevel <* token Dedent)) <?> "indented statements"
    -- Grammar hack so that Let statements slurp up the remainder
    -- of the block into their scope.
    , withSourceRange (
        ((\xs x -> mkBlock (xs ++ [x]))
         <$> (token Indent *> many toplevel)
         <*> (letStatement <* token Dedent)) <?> "indented statements")
    ]

  letStatement <- rule $
    withSourceRange (
     mkLet <$> ident
           <*> (token Colon *> typ)
           <*> (token LeftArrow *> value <* token Newline)
           <*> (withSourceRange (mkBlock <$> blockTail)) <?> "variable initialization")

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* token Newline
    , withSourceRange
      (mkIfThenElse <$> (token If *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , withSourceRange
      (mkWhile <$> (lit "while" *> value <* token Newline)
               <*> block) <?> "while loop"
    , withSourceRange
      (mkDoWhile <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "while" *> value <* token Newline))
    , withSourceRange
      (mkUntil <$> (lit "until" *> value <* token Newline)
               <*> block) <?> "until loop"
    , withSourceRange
      (mkDoUntil <$> (lit "repeat" *> token Newline *> block)
                 <*> (lit "until" *> value <* token Newline))
    , effect <?> "extended operation"
    ]

  elseIf <- ruleChoice
    [ ((token Else *> token Newline) *> block) <?> "else clause"
    , withSourceRange
      (mkIfThenElse <$> ((token Else *> token If) *> value <* (token Then <* token Newline))
                    <*> block
                    <*> elseIf) <?> "else if clause"
    , withSourceRange (pure noOp) <?> "end of block"
    ]

  let lit = token . Identifier
      noOp = \_sr -> EnvCode $ \env -> withEnvironment env $ pure NoOp

  simpleStatement <- ruleChoice
    [ withSourceRange
      (mkSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , withSourceRange (noOp <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammar splices

  effect <- ruleChoice
    [ toplevelDrawCommand
    , listCommand
    ]

  toplevelDrawCommand <- ruleChoice
    [ (tok "draw" *> drawCommand <* token Newline)
    , (tok "use" *> penCommand <* token Newline)
    , (tok "erase" *> eraseCommand <* token Newline)
    ]

  eraseCommand <- ruleChoice [withSourceRange (pure mkClear)]

  drawCommand <- ruleChoice
    [ withSourceRange
      (mkDrawPoint <$> ((tok "point" *> tok "at") *> value))
    , withSourceRange
      (mkDrawCircle <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "circle" *> tok "at") *> value)
                    <*> ((tok "with" *> tok "radius") *> value))
    , withSourceRange
      (mkDrawRect <$> (isJust <$> optional (tok "filled"))
                  <*> ((tok "rectangle" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    , withSourceRange
      (mkDrawLine <$> ((tok "line" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ withSourceRange (mkSetFill   <$> (value <* (tok "for" *> tok "fill")))
    , withSourceRange (mkSetStroke <$> (value <* (tok "for" *> strokeOrLine)))
    ]

  startOrEnd <- ruleChoice
    [ tok "start" $> Start
    , tok "end" $> End ]

  listCommand <- ruleChoice
    [ withSourceRange (mkListInsert
      <$> (tok "insert" *> value <* tok "at")
      <*> (startOrEnd <* tok "of")
      <*> (ident <* token Newline))
    , withSourceRange (mkListRemoveSome
      <$> (tok "remove" *> tok "each" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "from" *> ident <* token Newline))
    , withSourceRange (mkListRemoveAll
      <$> (tok "remove" *> tok "all" *> tok "items" *> tok "from" *> ident <* token Newline))
    , withSourceRange (mkListFor
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block))
    , withSourceRange (mkListWith
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (tok "do" *> token Newline *> block)
      <*> optional (tok "else" *> token Newline *> block))
    ]
  pure toplevel

atEnv :: EnvironmentProxy env -> EnvCode -> TC (Code env)
atEnv env (EnvCode f) = f env

mkBlock :: [EnvCode] -> SourceRange -> EnvCode
mkBlock body _ = EnvCode $ \env ->
  withEnvironment env $ do
    Block <$> traverse (atEnv env) body

mkLet :: String -> FSType -> ParsedValue -> EnvCode -> SourceRange -> EnvCode
mkLet n t v c sr = EnvCode $ \env -> withType t $ \ty ->
  withEnvironment env $ do
    -- Get a proof that `name` is not already bound in the environment `env`.
    -- Make an extended environment env' that binds name to ty
    -- Evaluate c in this extended environment.
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv' name env of
      Found' {} -> throwError (AlreadyDefined sr n)
      Absent' absent -> recallIsAbsent absent $ do
        let pf = bindName name ty absent
        val <- atType v ty
        let env' = BindingProxy name ty env
        Let pf name val <$> atEnv env' c

mkSet :: String -> ParsedValue -> SourceRange -> EnvCode
mkSet n v sr = EnvCode $ \env ->
  withEnvironment env $ do
    SomeSymbol name <- pure (someSymbolVal n)
    case lookupEnv' name env of
      Found' ty pf -> Set pf name <$> withKnownType ty (atType v ty)
      Absent' _ -> throwError (MissingName sr n)

mkWhile :: ParsedValue -> EnvCode -> SourceRange -> EnvCode
mkWhile cond body _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> atType cond BooleanType <*> atEnv env body)
               <*> pure NoOp

mkDoWhile :: EnvCode -> ParsedValue -> SourceRange -> EnvCode
mkDoWhile body cond _sr = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> atType cond BooleanType <*> atEnv env body

mkUntil :: ParsedValue -> EnvCode -> SourceRange -> EnvCode
mkUntil cond body _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> (DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body)
               <*> pure NoOp

mkDoUntil :: EnvCode -> ParsedValue -> SourceRange -> EnvCode
mkDoUntil body cond _sr = EnvCode $ \env -> do
  withEnvironment env $
    DoWhile <$> (Not <$> atType cond BooleanType) <*> atEnv env body

mkIfThenElse :: ParsedValue -> EnvCode -> EnvCode -> SourceRange -> EnvCode
mkIfThenElse cond yes no _sr = EnvCode $ \env -> do
  withEnvironment env $
    IfThenElse <$> atType cond BooleanType
               <*> atEnv env yes
               <*> atEnv env no

tcPoint :: KnownEnvironment env => ParsedValue -> TC (Value '(env, 'Pair 'RealT 'RealT))
tcPoint p@(ParsedValue sr _) =
  tryEach (Surprise sr "this"
            "not a complex number or pair of real numbers"
            (Expected "something point-like"))
    [ atType p (PairType RealType RealType)
    , C2R2 <$> atType p ComplexType ]

mkDrawPoint :: ParsedValue
            -> SourceRange
            -> EnvCode
mkDrawPoint v _sr = EnvCode $ \env -> withEnvironment env $
  (DrawCommand . DrawPoint env <$> tcPoint v)

mkDrawCircle :: Bool
             -> ParsedValue
             -> ParsedValue
             -> SourceRange
             -> EnvCode
mkDrawCircle isFilled center radius _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawCircle env isFilled <$> atType radius RealType <*> tcPoint center)

mkDrawRect :: Bool
           -> ParsedValue
           -> ParsedValue
           -> SourceRange
           -> EnvCode
mkDrawRect isFilled ul lr _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawRect env isFilled <$> tcPoint ul <*> tcPoint lr)

mkDrawLine :: ParsedValue
           -> ParsedValue
           -> SourceRange
           -> EnvCode
mkDrawLine ul lr _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand <$> (DrawLine env <$> tcPoint ul <*> tcPoint lr)

mkSetStroke :: ParsedValue -> SourceRange -> EnvCode
mkSetStroke c _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand . SetStroke env <$> atType c ColorType

mkSetFill :: ParsedValue -> SourceRange -> EnvCode
mkSetFill c _sr = EnvCode $ \env -> withEnvironment env $
  DrawCommand . SetFill env <$> atType c ColorType

mkClear :: SourceRange -> EnvCode
mkClear _sr = EnvCode (\env -> withEnvironment env $ pure (DrawCommand $ Clear env))

mkListInsert :: ParsedValue -> StartOrEnd -> String -> SourceRange -> EnvCode
mkListInsert item soe listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> throwError (MissingName sr listName)
    Found' listTy pfListPresent -> case listTy of
      ListType itemTy -> do
        Insert pfListPresent list listTy env soe <$> atType item itemTy
      ty -> notList sr listName ty

mkListRemoveSome :: String -> ParsedValue -> String -> SourceRange -> EnvCode
mkListRemoveSome itemName predicate listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> throwError (MissingName sr listName)
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {} -> throwError (AlreadyDefined sr itemName)
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ case listTy of
          ListType _ -> do
            Remove pfListPresent list listTy item pfItemAbsent env
              <$> atType predicate BooleanType
          ty -> notList sr listName ty

mkListRemoveAll :: String -> SourceRange -> EnvCode
mkListRemoveAll listName sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> throwError (MissingName sr listName)
    Found' listTy pfListPresent -> case listTy of
      ListType _ -> pure (ClearList pfListPresent list listTy env)
      ty -> notList sr listName ty

mkListFor :: String -> String -> EnvCode -> SourceRange -> EnvCode
mkListFor itemName listName body sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> throwError (MissingName sr listName)
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {} -> throwError (AlreadyDefined sr itemName)
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ do
          case listTy of
            ListType itemTy -> do
              let env' = declare itemTy env
              ForEach pfListPresent list listTy item pfItemAbsent env env' <$> atEnv env' body
            ty -> notList sr listName ty

mkListWith :: String
           -> ParsedValue
           -> String
           -> EnvCode
           -> Maybe EnvCode
           -> SourceRange
           -> EnvCode
mkListWith itemName predicate listName body fallback sr =
  EnvCode $ \env -> withEnvironment env $ do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  case lookupEnv' list env of
    Absent' _ -> throwError (MissingName sr listName)
    Found' listTy pfListPresent -> do
      case lookupEnv' item env of
        Found' {}  -> throwError (AlreadyDefined sr itemName)
        Absent' pfItemAbsent -> recallIsAbsent pfItemAbsent $ do
          case listTy of
            ListType itemTy -> do
              let env' = declare itemTy env
              Lookup pfListPresent list listTy item pfItemAbsent env' env
                <$> atType predicate BooleanType
                <*> atEnv env' body
                <*> traverse (atEnv env) fallback
            ty -> notList sr listName ty

notList :: SourceRange -> String -> TypeProxy ty -> TC a
notList sr name ty = throwError $
  Surprise sr name (an $ SomeType ty) (Expected "a list")
