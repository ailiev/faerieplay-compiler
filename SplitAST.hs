-- code to split the AST into several namespaces of global bindings:
-- constants, types, variables and functions

module SplitAST where

import qualified Data.Map as Map (Map, insert, empty)
import Control.Monad.State (MonadState, State, StateT, modify, runStateT)
import Control.Monad.Error (Error, throwError, catchError, noMsg, strMsg)
import Control.Monad.Trans (lift)

import SashoLib ((<<))

-- the abstract syntax tree description
import qualified SFDL.Abs as AST


-- This is the type of our type error representation.
data TypeError = Err {line::Int, reason::String}

-- We make it an instance of the Error class
instance Error TypeError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s


-- For our monad type constructor, we use Either TypeError
-- which represents failure using Left TypeError or a
-- successful result of type a using Right a.
-- note that we now have a type "TypeMonad a", synonym for "Either TypeError a"
type TypeMonad = Either TypeError


type StateWithErr = StateT SymbolTable TypeMonad

data DecType = Const | Type | Var | Func deriving (Eq,Ord,Show)

type SymbolTable = Map.Map (DecType,String) AST.Dec 


throwErr :: Int -> String -> StateWithErr ()
throwErr p msg = lift $ Left $ Err p msg

-- build the symbol table based on the list of declarations
collectDecs :: [AST.Dec] -> Either TypeError SymbolTable
collectDecs decs = let st = runStateT (addBindings decs) Map.empty
                       in case st of
                         Left err               -> Left err
                         Right (_, symtab)      -> Right symtab




-- add bindings to the "symbol table"
addBindings :: [AST.Dec] -> StateWithErr ()
addBindings decls = do mapM_ addBinding decls
                       throwErr 23 "top level fake"
--                        mapM_ addBinding decls


-- add a single binding
addBinding :: AST.Dec -> StateWithErr ()
addBinding = modify . mkModify
    where mkModify :: AST.Dec -> (SymbolTable -> SymbolTable)
          mkModify dec = Map.insert (getDeclId dec) dec


-- get the kind and name of a declaration
getDeclId :: AST.Dec -> (DecType, String)
getDeclId (AST.ConstDecl (AST.Ident id) _)            = (Const, id)
getDeclId (AST.TypeDecl (AST.Ident id) _)             = (Type, id)
getDeclId (AST.VarDecl _ ((AST.Ident id):[]))         = (Var, id)
getDeclId (AST.FunDecl _ (AST.Ident id) _ _ _)        = (Func, id)


testDecs = [ AST.ConstDecl (AST.Ident "x") (AST.EInt 10),
             AST.TypeDecl  (AST.Ident "y") AST.BoolT ]
