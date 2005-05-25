-- code to split the AST into several namespaces of global bindings:
-- constants, types, variables and functions

module SplitAST where

import Monad (zipWithM_)

import qualified Data.Map as Map
import Control.Monad.State (State, modify, runState)

-- the abstract syntax tree description
import qualified SFDL.Abs as Abs


data DecType = Const | Type | Var | Func deriving (Eq,Ord,Show)

type BindsMap = Map.Map (DecType,String) Abs.Dec 


-- build the symbol table based on the list of declarations
collectDecs :: [Abs.Dec] -> BindsMap
collectDecs decs = snd $ runState (addBindings decs) Map.empty


-- add bindings to the "symbol table"
addBindings :: [Abs.Dec] -> State BindsMap ()
addBindings decls = mapM_ addBinding decls

-- add a single Dec
addBinding = modify . mkModify
    where mkModify :: Abs.Dec -> (BindsMap -> BindsMap)
          mkModify dec = Map.insert (analyzeDecl dec) dec


-- how to deal with multiple id's in a VarDecl. geez
-- mkModify dec@(VarDecl typ ids) = zip (map getIdName ids)
--                                          (map (VarDecl typ . (:[])) ids)


analyzeDecl :: Abs.Dec -> (DecType, String)
analyzeDecl (Abs.ConstDecl (Abs.Ident id) _)            = (Const, id)
analyzeDecl (Abs.TypeDecl (Abs.Ident id) _)             = (Type, id)
analyzeDecl (Abs.VarDecl _ ((Abs.Ident id):[]))                 = (Var, id)
analyzeDecl (Abs.FunDecl _ (Abs.Ident id) _ _ _)        = (Func, id)
