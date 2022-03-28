module Types where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Data.Char
import Data.List

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

import Data.IORef

{-
  The following are the definitions of the 
  basic & composite data types of the language.

  Note: 
    Symbols are Lisp identifiers
    
    EKey are keywords in the lisp sense, which are just symbols 
    which always evaluate to themselves. As such they make really 
    good enums... I think.

    I've spent more significantly more time writting lisp 
    interpreters than writing in lisp itself, so its a bit strange
-}

type Symbol = String

data Expr
  = ENil
  | EBool Bool
  | EInt Int
  | EFloat Float
  | EChar Char
  | EStr String
  | ESym Symbol
  | EKey Symbol
  | EErr String
  | EFun Function
  | ELam Lambda
  | EMac Lambda
  | EList [Expr]

prettyType :: Expr -> String
prettyType val = case val of
  ENil     -> "Nil"
  EBool _  -> "Bool"
  EInt _   -> "Int"
  EFloat _ -> "Float"
  EChar _  -> "Char"
  EStr _   -> "String"
  ESym _   -> "Symbol"
  EKey _   -> "Keyword" 
  EErr _   -> "Exception"
  EFun _   -> "Function"
  ELam _   -> "Lambda"
  EMac _   -> "Macro"
  EList _  -> "List"

prettyExpr :: Expr -> String
prettyExpr val = case val of
  ENil        -> "nil"
  EBool b     -> map toLower $ show b
  EInt n      -> show n
  EFloat f    -> show f
  EChar chr   -> "#" ++ [chr]
  EStr str    -> "\"" ++ str ++ "\""
  ESym sym    -> sym
  EKey key    -> ":" ++ key
  EErr msg    -> msg
  EFun _      -> "#function"
  ELam lam    -> "#lambda:" ++ prettyLambda lam
  EMac lam    -> "#macro:" ++ prettyLambda lam
  EList elems -> "(" ++ unwords (map prettyExpr elems) ++ ")"

prettyVal :: Expr -> String
prettyVal val = prettyType val ++ " : " ++ prettyExpr val

-- | Builtin Functions
type Function = IORef Closure -> [Expr] -> Runtime Expr
data Arity = Fixed Int | Variadic Int


{-
  The difference between a function and a macro in Lisp has more
  do with how the arguments are fed into the function, rather 
  than how functions themselves work. So we can use the exact same
  internal representation.

  The only difference from the perspective of Expr, is that
  macros are tagged with EMac, and functions are tagged ELam

  Builtin functions are tagged EFun however, because they are 
  wrappers around Haskell functions, unlike macros/user defined
  functions (which are really just ASTs and not actual functions)
-}

-- | User defined function/macro
data Lambda = Lambda { 
    lambdaArity    :: Arity,
    lambdaArgs     :: [String],
    lambdaBody     :: Expr,
    lambdaClosure  :: Closure
  }

prettyLambda :: Lambda -> String
prettyLambda (Lambda arity args _ _) = case arity of 
  Fixed n    -> "[" ++ inner ++ "]"
  Variadic n -> "variadic[" ++ inner ++ "]"
  where inner = intercalate ", " args


{-
  The Closure of a lambda is the set of all possible variables it could 
  potentially capture.
  
  However, since we use lexical scoping, it is possible to statically infer 
  precisely which variables need to be captured. But that's outside the 
  scope of this project (Atleast for now).
-}

data Closure = Closure {
    closureLocals :: Map Symbol Expr,
    closureParent :: Maybe (IORef Closure)
  }

-- | Searches the scopes recursively upward (starting from local)
closureGet :: Symbol -> Closure -> IO (Maybe Expr)
closureGet sym (Closure local parent) = case Map.lookup sym local of
  Just ex -> pure $ Just ex
  Nothing -> case parent of 
    Nothing    -> pure Nothing
    Just outer -> readIORef outer >>= closureGet sym

-- | Modifies the local scope only
closureSet :: Symbol -> Expr -> Closure -> Closure
closureSet sym val closure = closure {
  closureLocals = Map.insert sym val $ closureLocals closure }


{-
  The Runtime Monad stack represents IO computations that can fail with an 
  error message. Which is in essence, the type of imperative function 
  (In a single threaded model, and there are probably some minor caveats)

  This took quite a while to figure out, Initially I had more nested stack,
    
    type Runtime a = StateT Closure (ExceptT Expr IO) a

  It took me a surprising amount of time to notice that this essentially
  created a static snapshot of the enviroment when I pass it to a lambda,
  instead of the intended behaviour (which was to pass a dynamic mutable reference)

  The mutablity is pretty important because otherwise we can't define
  recursive functions. So if we had something like the following

    (def! loop-greet 
      (lambda [] { do        
        (println! "hello")
        (loop-greet)}))

  The lambda first captures the enviroment, then it is stored in loop-greet. 
  So if it only has a snapshot rather than a live reference, it wont be able 
  to call itself
-}

type Runtime a = ExceptT Expr IO a

{-
  The following functions just lifts the closure getter/setter up to 
  the Runtime stack
-}

getSym :: String -> IORef Closure -> Runtime (Maybe Expr)
getSym sym ref = do 
  closure <- lift $ readIORef ref
  lift $ closureGet sym closure

setSym :: IORef Closure -> String -> Expr -> Runtime ()
setSym env sym val = lift $ modifyIORef' env $ closureSet sym val

{-
  The following are some helper functions to simply error handling. But
  they're also there so that I can easily change the order of the Monad
  stack, without having to change much code (which I did end up doing, 
  as mentioned previously).
-}

raiseExpr :: Expr -> Runtime a
raiseExpr = throwE 

raise :: String -> Runtime a 
raise str = throwE $ EErr str

typeError :: String -> [Expr] -> String -> Runtime a
typeError name actual expected = raise $ concat [
    name, " : expected ", expected, 
    ", got ", proj prettyType, 
    " from arguments ", proj prettyExpr
  ]
  where 
    proj fn = "(" ++ intercalate ", " (map fn actual) ++ ")"
