{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

import           Data.Char
import           Data.IORef
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

import           System.IO

import           Parser
import           Types
import           Core

{-
  Note: 
    The exposition is in a bit of a weird order, 
    so ideally one should read the files in this order

    Types.hs -> Parser.hs -> Main.hs
             -> Core.hs 
         
-}

{-
  Checks if the parameter list is well constructed

    (lambda/macro [param list] body)
                  ^^^^^^^^^^^^
    
  Parameters should not repeat, and only the last one
  can have a variadic. 

    [x y ..z]
         ^^^ variadic binding
-}
validateParams :: [String] -> [Expr] -> Runtime [String]
validateParams usedParams = \case
  ESym name : rest -> do
    when (name `elem` usedParams) 
      $ raise $ name ++ " is a repeated parameter"

    when (".." `isPrefixOf` name && not (null rest))
      $ raise "only last parameter can have variadic prefix '..'"

    (name :) <$> validateParams (name : usedParams) rest
  val : rest -> raise $ "parameters must be symbols, not " ++ prettyVal val
  []         -> pure []

{- 
  As functions and macros have the same internal representation,
  we have only one internal type for constructing them
-}
makeLambda :: (Lambda -> Expr) -> IORef Closure -> [Expr] -> Runtime Expr
makeLambda constr env = \case
  [EList params, body] -> do
    params <- validateParams [] params
    let 
      closure = Closure Map.empty (Just env)
      len = length params
      arity
        | null params                   = Fixed len
        | ".." `isPrefixOf` last params = Variadic len 
        | otherwise                     = Fixed len
    pure $ constr $ Lambda arity params body closure
  args -> typeError "lambda" args "(List, Any)"

{-
  Evalute the let block

    (let 
      (sym-1 value-1)
      (sym-2 value-2)
      ...
      retvalue)
  
  The bindings are strictly ordered sequentially, and
  later bindings can depend on earlier bindings.
  
  Its similar in a way to the monadic do block
    do 
      sym1 <- val1
      sym2 <- val2
      ...
      return value
-}
evalLet :: IORef Closure -> [Expr] -> Runtime Expr
evalLet env []            = pure ENil
evalLet env [expr       ] = eval env expr
evalLet env (head : rest) = case head of
  EList [ESym name, val] -> do
    setSym env name val
    evalLet env rest
  EList args -> typeError "let binding" args "(Sym, Any)"
  xs         -> raise $ "Expected let binding, got " ++ prettyVal xs


{-
  evalInner evaluates an AST by evalutating the individual
  elements in the list

    (x   x   z   ...)
     ↓   ↓   ↓
    (x'  y'  z'  ...) 
    ^^^ not used as function

  It however, does not fully evaluate the expression; most notably
  by not invoking the first argument as a function
-}


evalInner :: IORef Closure -> Expr -> Runtime Expr
evalInner env (ESym sym) = getSym sym env >>= \case
  Just ex -> pure ex
  Nothing -> raise $ sym ++ " not found"
evalInner env (EList list) = EList <$> mapM (eval env) list
evalInner env other        = pure other

{-
  Quasiquote splices values into the AST in the following manner

  suppose x = 0, y = (5 6)
  then 
    `(1 2 ~x 3 ~x) -> (1 2 0 3 0)
    `(1 2 ~@y 3)   -> (1 2 5 6 3)

  where ` ~ ~@ are syntactic sugar (described in Parser.hs)

-}
quasiquote :: IORef Closure -> Expr -> Runtime Expr
quasiquote env ast@(EList [ESym "quasiquote", _]) = pure ast
quasiquote env ast@(EList xs) = do
  elems <- forM xs $ \case
    EList [ESym "unquote", val] -> 
      eval env val >>= (\x -> pure [x])
    EList [ESym "splice-unquote", val] -> do 
      eval env val >>= \case 
        EList xs -> pure xs
        other    -> typeError "splice-unquote" [other] "(List)" 
    other -> do 
      other' <- quasiquote env other 
      pure [other']
  
  pure $ EList $ concat elems

quasiquote env other = pure other

{-
  Macroexpand takes an AST of the following form

    (mymacro a b ... )
     ^^^^^^^

  looks up the first value to see if it is a macro,
  and if so, expands it (taking a b ... as arguments)
-}
macroexpand :: IORef Closure -> Expr -> Runtime (Maybe Expr)
macroexpand env ast@(EList (ESym sym : args)) = do 
  getSym sym env >>= \case 
    Just (EMac macro) -> do 
      inner <- liftIO $ newIORef (lambdaClosure macro)
      bindArguments inner args macro
      Just <$> eval inner (lambdaBody macro)
    _ -> pure Nothing
macroexpand env other = pure Nothing


-- | repeatedly calls `macroexpand` until the AST is fully expanded
recmacroexpand :: IORef Closure -> Expr -> Runtime Expr
recmacroexpand env ast = do
  macroexpand env ast >>= \case 
    Just ast' -> recmacroexpand env ast'
    Nothing   -> pure ast 

{-
  evalApply takes a fully expanded AST and tries to 
  evaluate it as either a special form or as a function

  Most of the function is just responsible for pattern 
  matching against the special forms, and handling
  any errors in doing so 
  (mismatched types or insufficient arguments etc).
-}

evalApply :: IORef Closure -> Expr -> Runtime Expr
evalApply env ast@(EList (head : body)) = case head of
  ESym "lambda" -> makeLambda ELam env body
  ESym "macro"  -> makeLambda EMac env body

  ESym "quasiquote" -> case body of 
    [ast] -> quasiquote env ast
    _ -> typeError "quasiqutoe" body "1 argument"
  ESym "quote"  -> case body of
    [ast] -> pure ast
    _   -> typeError "quote" body "1 argument"

  ESym "macroexpand" -> case body of 
    [ast] -> macroexpand env ast >>= \case
      Just ast' -> pure ast'
      Nothing   -> pure ast
    _   -> typeError "macroexpand" body "1 argument"

  ESym "let" -> do
    closure <- liftIO $ newIORef $ Closure Map.empty (Just env)
    evalLet closure body

  ESym "do" -> case body of 
    [] -> pure ENil
    ast -> last <$> mapM (eval env) ast 

  ESym "if" -> case body of
    [val, x, y] -> do 
      cond <- eval env val
      case cond of 
        EBool True -> eval env x
        EBool False -> eval env y
        _ -> typeError "if condition" [cond] "(Bool)" 
    _           -> typeError "if" body "3 arguments"
  
  ESym "catch" -> case body of 
    [ast] -> do 
      result <- lift $ runExceptT (eval env ast) 
      pure $ case result of 
        Left err -> EList [EKey "Err", err]
        Right val -> EList [EKey "Ok", val]
    _ -> typeError "catch" body "1 argument"

  ESym "def" -> case body of
    [ESym sym, val] -> ENil <$ (eval env val >>= setSym env sym)
    _               -> typeError "def" body "(Sym, Any)"

  _ -> do    
    evalInner env ast >>= \case
      EList (EFun fn : args) -> fn env args
      EList (ELam lambda : args) -> do
        inner <- liftIO $ newIORef (lambdaClosure lambda)
        bindArguments inner args lambda        
        eval inner (lambdaBody lambda)
      x -> raise $ prettyVal x ++ " is not a function"
evalApply env other = evalInner env other

{-
  eval takes any AST, fully expands all macros in it and then
  evaluates it as a function. 
-}
eval :: IORef Closure -> Expr -> Runtime Expr
eval env expr = recmacroexpand env expr >>= evalApply env

{-
  Takes a list of arguments and a lambda,
  and tries to call said function with the argument

  The function throws an (interpreter) exception if
  number of arguments does not match the required amount

  It handles both variadic/fixed-arity and lambda/macros,
-}
bindArguments :: IORef Closure -> [Expr] -> Lambda -> Runtime ()
bindArguments env args lambda@(Lambda arity params body inner) = do
  boundArgs <- case arity of 
    Fixed n    -> do 
      when (n /= argLen) $ argErr (show n) 
      pure args
    Variadic n -> do 
      when (n > argLen) $ argErr (">=" ++ show n)
      let (begin, end) = splitAt (n - 1) args
      pure $ begin ++ [EList end]

  forM_ (zip params boundArgs) $ \(name, val) -> do
    setSym env name val
  where  
    argLen = length args
    argErr expected = raise $ concat 
      [prettyLambda lambda, " expected ", expected, ", got ", show argLen, " arguments"]


{-
  REPL : Read-Eval-Print-Loop,
  in other words, it is an interactive prompt
-}

repl :: IORef Closure -> IO ()
repl env = do
  line <- putStr "λ " >> hFlush stdout >> getLine
  case parse parseRepl "repl" line of
    Right (Just (ESym "quit")) -> pure ()
    Right Nothing              -> repl env
    Left  syntaxErrors         -> do
      putStrLn $ errorBundlePretty syntaxErrors
      repl env

    Right (Just expr) -> do
      result <- runExceptT (eval env expr)
      case result of
        Left err -> putStrLn $ case err of
          (EErr msg) -> "Error: " ++ msg
          err        -> "Error: " ++ prettyVal err
        Right ENil -> pure ()
        Right val  -> putStrLn $ prettyVal val
      repl env

{-
  The following are a few builtin functions that were easier to 
  declare here (instead of Core.hs) since they require eval
-}

fnEval :: Function
fnEval env args = case args of
  [val] -> eval env val
  args  -> typeError "eval" args "1 argument"

fnParse :: Function 
fnParse env args = case args of 
  [EStr src] -> do 
    case parse parseProgram "meta" src of 
      Left syntaxErrors -> raise $ errorBundlePretty syntaxErrors
      Right exprs -> pure $ EList (ESym "do" : exprs)
  _ -> typeError "parse" args "(String)"

fnImport :: Function 
fnImport env args = do 
  file <- fnLoadFile env args 
  ast <- fnParse env [file]
  eval env ast


-- | List of functions that are included by default
prelude :: Closure
prelude = Closure (Map.fromList vars) Nothing
 where
  vars = [ 
      "throw" #> fnThrow,

      "print" #> makePrint putStr, 
      "println" #> makePrint putStrLn, 
    
      "read" #> fnRead,
      "load" #> fnLoadFile,

      "+" #> fnAdd, "ADD" #> fnAdd,
      "-" #> fnSub, "SUB" #> fnSub,
      "*" #> fnMul, "MUL" #> fnMul,
      "/" #> fnDiv, "DIV" #> fnDiv,

      "<" #> fnLT, "<=" #> fnLTE,
      ">" #> fnGT, ">=" #> fnGTE,
      "=" #> fnEq,
      
      "not" #> fnNot, 
      "and" #> fnAnd, 
      "or" #> fnOr, 

      "cons" #> fnCons,
      "head" #> fnHead, 
      "tail" #> fnTail,

      "eval" #> fnEval,
      "parse" #> fnParse,
      "import" #> fnImport
    ]

  str #> fn = (str, EFun fn)

main :: IO ()
main = do
  putStrLn "(Haskell] 0.0.1"
  newIORef prelude >>= repl
