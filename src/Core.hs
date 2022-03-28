module Core where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Types

import           System.Directory


{-
  The following are the Core builtin functions of the language. Since
  they are pretty self explanatory, the documentation in this module
  will be a bit sparse.

  All the functions here rely on destructuring their inputs internally,
  since they need to have the same type (because they need to be stored
  in Expr)
-}

fnCons :: Function 
fnCons env xs = case xs of 
  [x, EList xs] -> pure $ EList (x : xs)
  _             -> typeError "cons" xs "(Any, List)"

fnHead :: Function
fnHead env xs = case xs of
  [EList (x : _)] -> pure x
  [EList []     ] -> raise "head : empty list"
  _               -> typeError "head" xs "(List)"

fnTail :: Function
fnTail env xs = case xs of
  [EList (_ : xs)] -> pure $ EList xs
  [EList []      ] -> raise "tail : empty list"
  _                -> typeError "tail" xs "(List)"


{-
  Here we wrap a bunch of numeric operators to be accessible
  to the interpreter

  This seems a bit hacky, but there doesnt seem to be any
  obvious way to clean it up.
-}

numFn :: String -> (Int -> Int -> Expr) -> (Float -> Float -> Expr) -> Function
numFn name int float = fn
  where
    fn env xs = case xs of 
      [EInt x, EInt y]     -> pure $ x `int` y
      [EInt x, EFloat y]   -> pure $ fromIntegral x `float` y
      [EFloat x, EInt y]   -> pure $ x `float` fromIntegral y
      [EFloat x, EFloat y] -> pure $ x `float` y
      _                    -> typeError name xs "(Int/Float, Int/Float)"
    

arith :: String -> (Int -> Int -> Int) -> (Float -> Float -> Float) -> Function
arith name int float = numFn name (\x y -> EInt $ int x y) (\x y -> EFloat $ float x y) 

fnAdd, fnSub, fnMul, fnDiv :: Function
fnAdd = arith "ADD" (+) (+)
fnSub = arith "SUB" (-) (-)
fnMul = arith "MUL" (*) (*)
fnDiv = arith "DIV" div (/)

comp :: String -> (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> Function
comp name int float = numFn name (\x y -> EBool $ int x y) (\x y -> EBool $ float x y)

fnLT, fnLTE, fnGT, fnGTE :: Function
fnLT  = comp "<"  (<)  (<)
fnLTE = comp "<=" (<=) (<=)
fnGT  = comp ">"  (>)  (>)
fnGTE = comp ">=" (>=) (>=)

fnEq :: Function 
fnEq env xs = EBool <$> case xs of 
  [x, y] -> do 
    checkComplex x
    checkComplex y

    {-
      This also needs some fixing, but it is non trivial to 
      just derive an Eq instance due to the presence of functions 
      in Expr. Might be avoidable in TemplateHaskell

      For that reason only, I have decided to set them to throw
      exceptions for any complex types.
    -}

    if prettyType x /= prettyType y
      then pure False
      else pure $ case (x, y) of 
        (ENil,     ENil)     -> True
        (EBool x,  EBool y)  -> x == y
        (EInt x,   EInt y)   -> x == y
        (EFloat x, EFloat y) -> x == y
        (EChar x,  EChar y)  -> x == y
        (EStr x,   EStr y)   -> x == y
        (ESym x,   ESym y)   -> x == y
        (EKey x,   EKey y)   -> x == y
        _ -> False

  _ -> False <$ typeError "=" xs "(Any, Any)"
  where 
    checkComplex x = when (prettyType x `notElem` 
      ["Nil", "Bool", "Int", "Float", "Char", "String", "Symbol", "Keyword"]) $
      raise $ "= is undefined on " ++ prettyVal x


{-
  I'm not sure if I made a mistake somewhere, but
  it seems like throw can be implemented as a
  regular function, but catch needs to be implemented
  as a builtin. Interesting?
-}

fnThrow:: Function
fnThrow env xs = case xs of
  [x] -> raiseExpr x
  _   -> typeError "throw" xs "(Any)"


fnNot :: Function
fnNot env xs = case xs of
  [EBool x] -> pure $ EBool $ not x
  _         -> typeError "NOT" xs "(Bool)"

fnAnd :: Function
fnAnd env xs = case xs of
  [EBool x, EBool y] -> pure $ EBool $ x && y
  _                  -> typeError "AND" xs "(Bool, Bool)"

fnOr :: Function
fnOr env xs = case xs of
  [EBool x, EBool y] -> pure $ EBool $ x || y
  _                  -> typeError "OR" xs "(Bool, Bool)"


{-
  Symbols and Strings become somewhat ambiguous under this conversion,
  (Although technically speaking, a string without quotes could be 
  confused for anything, so its not unique to Symbols)

  I think most people would prefer a little ambiguity over 
  strings being printed with quotes.
-}

makePrint :: (String -> IO ()) -> Function
makePrint prn env xs = ENil <$ liftIO (prn $ concatMap pretty xs)
 where
  pretty (EStr x) = x
  pretty x        = prettyExpr x

fnRead :: Function 
fnRead env args = case args of 
  [] -> EStr <$> liftIO getLine
  _ -> typeError "read" args "()"


fnLoadFile :: Function 
fnLoadFile env args = case args of 
  [EStr path] -> do
    exists <- liftIO $ doesFileExist path

    unless exists $ raise $ path ++ " does not exist"
  
    src <- liftIO $ readFile path 
    pure $ EStr src

  _ -> typeError "load" args "(String)"

