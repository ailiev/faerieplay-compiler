module Runtime where

import Maybe
import Monad
import Ix
import List

import Common (trace)

import Array

{-
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Query.BFS as GrBF
import qualified Data.Graph.Inductive.Tree      as TreeGr
-}

import qualified Data.Array.MArray              as MArr
import qualified Data.Array.IO                  as IOArr

import qualified Debug.Trace                    as Trace

import Text.Printf      (printf)

import SashoLib

import CircGen
import Intermediate


-- general tactic:
-- produce NodeFunc's for each gate, which is a simple map.
-- go through graph with BFS and do the funcs

formatRun gates ins = do out <- run gates ins
                         return $ map printOuts out
    where printOuts (num, (op,flags), ins, val) = printf "%-6d%-16s%-8s%-30s%s"
                                                    num
                                                    (show op)
                                                    (if null flags then "" else show flags)
                                                    (concatMap ((++ ", ") . show) ins)
                                                    (show val)


-- run :: [Gate]       -- ^ The circuit
--     -> [Integer]    -- ^ input values
--     -> IO [(Int, (Op,[GateFlags]), GateVal)]    -- ^ values and numbers for all the gates
run gates ins   = do let range = (0, length gates - 1)
                     vals      <- (MArr.newArray range Blank)
                               :: IO (IOArr.IOArray Int GateVal)
                     gateArr   <- (MArr.newListArray range $ zip gates (map gate2func gates))
                               :: IO (IOArr.IOArray Int (Gate, NodeFunc))
                     setRoots gateArr vals (map (VScalar . ScInt) ins)
                     evalGates gateArr vals
                     -- need the values sorted in gate order.
                     let gate_idxs = map gate_num gates
                     vals_sorted <- mapM (MArr.readArray vals) gate_idxs
                     -- the inputs to each gate
                     inputs      <- mapM (mapM (MArr.readArray vals)) (map gate_inputs gates)
                     return $ zip4 gate_idxs
                                   (map (pair2 gate_op gate_flags)
                                        (gates))
                                   inputs
                                   vals_sorted

-- get the input values 
getInputs vals in_idxs = mapM (MArr.readArray vals) in_idxs


-- set the values of root gates, ie. those without input gates.
setRoots gates vals ins = do assocs <- MArr.getAssocs gates
                             -- should eat up all the inputs
                             []     <- foldM (addRoot vals) ins assocs
                             return ()

-- set the value of one root gate (if it is a root gate)
addRoot :: (MArr.MArray a GateVal m)
           => a Int GateVal
               -> [GateVal]
               -> (Int, (Gate,NodeFunc))
               -> m [GateVal]
addRoot vals ins (i, (gate,_)) =
    let g_i = gate_num gate in
    do case gate_op gate of
         Lit l -> do MArr.writeArray vals g_i (VScalar $ case l of LInt i     -> ScInt i
                                                                   LBool b    -> ScBool b)
                     return ins
                         `trace` ("Wrote lit to gate " << g_i)
         Input -> let (v:vs) = ins in
                  do MArr.writeArray vals g_i v
                     return vs
         _     -> return ins

evalGates :: (MArr.MArray a (Gate, NodeFunc) m,
              MArr.MArray a2 GateVal m)
             => a Int (Gate, NodeFunc)
                 -> a2 Int GateVal
                 -> m()
evalGates gates vals = mapM_ (evalGate gates vals) (MArr.indices gates)

evalGate :: (MArr.MArray a (Gate, NodeFunc) m,
              MArr.MArray a2 GateVal m)
             => a Int (Gate, NodeFunc)
                 -> a2 Int GateVal
                 -> Int
                 -> m()
evalGate gates vals i =
    do (gate, gate_f)   <- MArr.readArray gates i
       ins              <-  mapM (MArr.readArray vals) (gate_inputs gate)
                              `trace` ("reading inputs for gate " << gate)
       MArr.writeArray vals (gate_num gate) (gate_f ins)




data GateVal = Blank |
               VScalar   Scalar |
               VArr      (Array.Array Integer GateVal) |
               VList [GateVal]  -- for output of a ReadDynArray



data Scalar = ScInt Integer |
              ScBool Bool





type NodeFunc = ([GateVal] -> GateVal)

gate2func :: Gate -> NodeFunc
gate2func Gate { gate_op = op,
                 gate_flags = flags }
    = case op of
        Bin op              -> \[VScalar s1, VScalar s2]    -> VScalar $ binOpFunc op s1 s2
        Un  op              -> \[VScalar s]                 -> VScalar $ opUnOp op s
        Lit l               -> \[]                          -> case l of
                                                                 (LInt i) -> VScalar $ ScInt i
                                                                 (LBool b) -> VScalar $ ScBool b
        Select              -> \[ VScalar (ScBool b),
                                  v_true,
                                  v_false ]                 -> if b
                                                               then v_true
                                                               else v_false
        Slicer _ (off,len)  -> \[VList vs]                  -> head . take len . drop off $ vs
        WriteDynArray _     -> \( VArr arr:
                                  VScalar (ScInt idx):
                                  vals )                    -> VArr $ arr // [(idx, head vals)]
        InitDynArray _ len  -> \[]                          -> VArr $ listArray (0,len-1)
                                                                                (repeat $ VScalar $
                                                                                          ScInt 0)
        ReadDynArray        -> \[VArr arr,
                                 VScalar (ScInt idx)]       -> let val = arr ! idx in
                                                               VList [VArr arr,
                                                                      val]
        Input               -> \[]                          -> Blank
                                                                            
                                                                                   
                                  

binOpFunc o x y = let opClass                       = classifyBinOp o
                  in  case (opClass, x, y) of
                        (Arith, ScInt i1, ScInt i2)     -> ScInt $ arithBinOp o i1 i2
                        (Boolean, ScBool b1, ScBool b2) -> ScBool $ boolBinOp o b1 b2
                        (Comp, ScBool b1, ScBool b2)    -> ScBool $ compBinOp o b1 b2
                        (Comp, ScInt i1, ScInt i2)      -> ScBool $ compBinOp o i1 i2


arithBinOp o = case o of
                 Times -> (*) 
                 Plus  -> \x y -> x + y
                                    `trace`
                                     ("Adding " << x << " + " << y)
                 Div   -> div
                 Mod   -> mod
                 Minus -> (-) 
                 Max   -> max
boolBinOp o = case o of
                And -> (&&)
                Or  -> (||)
                
compBinOp o = case o of
              Lt    -> (<)
              Gt    -> (>) 
              LtEq  -> (<=)
              GtEq  -> (>=) 
              Eq    -> (==)
              Neq   -> (/=) 
                
{-
-}
--               SL -> 
--               SR -> ">>" 
--               BAnd -> "&" 
--               BXor -> "^" 
--               BOr -> "|" 

opUnOp :: UnOp -> (Scalar -> Scalar)
opUnOp o x = case o of
               Not  -> let (ScBool b) = x in ScBool $ not b
                    --                   BNot -> "~"
               Neg  -> let (ScInt i) = x in ScInt $ negate i
               Log2 -> let (ScInt i) = x in ScInt $ ilog2 i
             --                   Bitsize -> "bitsize"

data BinOpClass = Arith | Boolean | Comp

classifyBinOp o = case o of
                    Times -> Arith 
                    Div   -> Arith
                    Mod   -> Arith
                    Plus  ->  Arith
                    Minus ->  Arith
                    SL    ->  Arith
                    SR    ->  Arith
                    Lt    ->  Comp
                    Gt    ->  Comp
                    LtEq  ->  Comp
                    GtEq  ->  Comp
                    Eq    ->  Comp
                    Neq   ->  Comp
                    BAnd  ->  Arith
                    BXor  ->  Arith
                    BOr   ->   Arith
                    And   ->  Boolean
                    Or    -> Boolean
                    Max   -> Arith


instance (Show GateVal) where
    showsPrec p g = case g of
                      Blank     -> str "Blank"
                      VScalar s -> rec s
                      VArr arr  -> str "array[" . rec (rangeSize $ bounds arr) . str "]"
                      VList l   -> showList l
        where rec x = showsPrec p x
              str = (++)

instance (Show Scalar) where
    showsPrec p s = case s of (ScInt i)  -> showsPrec p i
                              (ScBool b) -> showsPrec p b

