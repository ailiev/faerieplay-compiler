{-# OPTIONS_GHC -cpp #-}
-- cpp because of the MArr.getBounds change from GHC 6.4.2 to 6.5

module Runtime where

import Maybe
import Monad
import Ix
import List
import IO               (hFlush, stdout)
import Common (trace, RunFlag(..))

import Array

{-
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Query.BFS as GrBF
import qualified Data.Graph.Inductive.Tree      as TreeGr
-}

import qualified Data.Array.MArray              as MArr
import qualified Data.Array.IO                  as IOArr
import           Control.Monad.Error    (MonadError(..))

import qualified Debug.Trace                    as Trace

import Data.Bits        ((.&.), (.|.), shiftL, shiftR, xor)

import Text.Printf      (printf)

import qualified Text.PrettyPrint               as PP
import Text.PrettyPrint                         (($$))

import SashoLib

import           CircGen                        as CG
import Intermediate                             as Im



type ValArrayType = Maybe GateVal


formatRun :: [RunFlag]          -- ^ The runtime flags
          -> [Gate]             -- ^ The graph gates in topological order
          -> [[Integer]]        -- ^ The values of the inputs, each elements may be a
                                -- singleton list for a scalar, and a list for arrays.
          -> IO [String]        -- ^ The trace of formatted values for each gate.
formatRun rtFlags gates ins = do outs <- run gates ins
                                 return $ map printOuts $ zip gates outs
    where printOuts = if elem RunTraceBinary rtFlags
                      then printOutsBinary
                      else printOutsHuman

          -- NOTE: this should be kept synchronized with the LOG to 'gate_logger' in
          -- run-circuit.cc.
          printOutsBinary ( g, (ins, mb_val) ) =
              printf "%-6d%-8s%s"
                     (gate_num g)
                     (if null (gate_flags g) then "" else show (gate_flags g))
                     (showsValBinary 0 mb_val "")

          printOutsHuman ( gate, (ins, mb_val) ) =
              let flags     = gate_flags gate
                  flagsStr  = if null flags then "" else show flags
                  insStr    = concat . intersperse ", " . map showVal $ ins
                  valStr    = if (elem Output flags)
                              then showVal mb_val
                              else showVal mb_val
                  doc       = PP.hsep   [PP.int (gate_num gate),
                                         PP.text $ strShow $ gate_op gate,
                                         PP.text flagsStr,
                                         PP.parens . PP.hcat . PP.punctuate PP.comma .
                                             map PP.int . gate_inputs $
                                             gate,
                                         PP.text insStr]
                              $$
                              PP.nest 6
                                    ({- PP.text insStr $$ -}
                                     PP.text valStr)
              in
                PP.render doc

          showVal v = showsValHuman 0 v ""


{-
              printf "%-6d%-16s%-8s%-30s%s"
                                                    num
                                                    (show op)
                                                    (if null flags then "" else show flags)
                                                    (concat . intersperse ", " . map show $ ins)
                                                    (if (elem Output flags)
                                                     then show_mb_val mb_val
                                                     else show_mb_val mb_val)
-}


-- run :: [Gate]       -- ^ The circuit
--     -> [[Integer]]  -- ^ input values - singleton array for scalar inputs, larger array
                       -- for array inputs.
--     -> IO [(Int, (Op,[GateFlags]), GateVal)]    -- ^ values and numbers for all the gates
run gates ins   = do let range = (0, length gates - 1)
                     valArr    <- (MArr.newArray range Nothing)
                                       :: IO (IOArr.IOArray Int ValArrayType)
                     gateArr   <- (MArr.newListArray range $ zip gates (map gate2func gates))
                                       :: IO (IOArr.IOArray Int (Gate, NodeFunc))
                     setRoots gateArr valArr ins
                     evalGates gateArr valArr
                     -- need the values sorted in gate order.
                     let gate_idxs = map gate_num gates
                     vals_sorted <- mapM (MArr.readArray valArr) gate_idxs
                     -- the inputs to each gate
                     inputs      <- mapM (mapM (MArr.readArray valArr)) (map gate_inputs gates)
                     return $ zip inputs
                                  vals_sorted

-- get the input values
-- getInputs vals in_idxs = mapM (MArr.readArray vals) in_idxs

-- prompt user for inputs, and return as a list of lists of integers:
-- a scalar value just gets a 1-element list,
-- an array value gets a list with an integer for each element
-- TODO: support scalar types other than int (structures)
getUserInputs :: [Gate] -> IO [[Integer]]
getUserInputs gates = let in_gates = filter (\g -> gate_op g == Input) gates
                      in  mapM getOneInput in_gates

    where getOneInput Gate { gate_typ = typ,
                             gate_doc = doc }
                      -- the first annotation (which is at the end of the list) is likely
                      -- to be the actual param name
              =   let docstr = show $ last doc
                  in  case typ of
                        (ArrayT _ size_e)   ->
                            let size  = evalStaticOrDie size_e
                            in  replicateM (fromIntegral size) (getInt (docstr << ", array of size " << size))
                        _                   ->
                            getInt docstr >>== (:[]) -- make into a singleton list

          getInt :: String -> IO Integer
          getInt doc    = do putStr ("Integer value for " ++ doc ++ ": ")
                             hFlush stdout
                             getLine >>== read >>= return



-- set the values of root gates, ie. those without input gates.
-- gates: the circuit gates
-- vals: the (blank) values array
-- ins: [[Integer]]
setRoots gates vals ins = do assocs <- MArr.getAssocs gates
                             -- should eat up all the inputs
                             (do []     <- foldM (addRoot vals) ins assocs
                                 return ())
                             `catchError`
                               (\e -> putStrLn "ERROR: Not the right number of input values provided")



-- set the value of one root gate (if it is a root gate)
-- TODO: only supporting plain integers (and arrays of them) for now. No structs.
addRoot :: (MArr.MArray a ValArrayType m) =>
                  a Int ValArrayType
               -> [[Integer]]
               -> (Int, (Gate,NodeFunc))
               -> m [[Integer]]
addRoot vals ins (i, (gate,_)) =
    let g_i = gate_num gate in
    do case gate_op gate of
         Lit l -> do MArr.writeArray vals g_i (Just $ VScalar $ case l of LInt i     -> ScInt i
                                                                          LBool b    -> ScBool b)
                     return ins -- just pass on the input value list
                         `trace` ("Wrote lit " << l << " to gate " << g_i)

         -- will swallow one of the 'ins' values here
         Input -> let 
                      typ       = gate_typ gate
                      (vs:ins') = ins `trace` ("Setting input value of gate " << gate
                                               << " of type " << typ)
                  in  do MArr.writeArray vals g_i
                          (case (typ `trace` ("Input type is " << typ)) of
                             (ArrayT _ size_e)   -> let size = evalStaticOrDie size_e
                                                        arr  = listArray (0,size-1)
                                                                         (map (Just . vint) vs)
                                                    in  (Just $ VArr arr)
                                                            {- `trace` ("Input array is "
                                                               << show arr) -}
                             _                   -> let [i] = vs -- should be just one integer
                                                    in  Just $ vint i )
                         -- BUG: from the compiler I think - if this statement is not
                         -- here, the previous writeArray is not evaluated!
                         val <- MArr.readArray vals g_i
                         return ins' `trace` ("vals after input added: " << showsValHuman 0 val "")
                                                                              
         _     -> return ins



evalGates :: (MArr.MArray a (Gate, NodeFunc) m,
              MArr.MArray a2 ValArrayType m) =>
                    a Int (Gate, NodeFunc)
                 -> a2 Int ValArrayType
                 -> m()
-- need this liftArgM here because getMArrIndices gives us the index list in the Monad.
evalGates gates vals = liftArgM (mapM_ (evalGate gates vals))
                                (getMArrIndices gates)

evalGate :: (MArr.MArray a      (Gate, NodeFunc) m,
             MArr.MArray a2     ValArrayType     m) =>
                   a  Int (Gate, NodeFunc)
                -> a2 Int ValArrayType
                -> Int
                -> m ()
evalGate gates vals i =
    do (gate, gate_f)   <- MArr.readArray gates i
       ins              <- mapM (MArr.readArray vals) (gate_inputs gate)
                              `trace` ("reading inputs for gate: " << gate)
       let out = gate_f ins
       -- no point in writing Blank values in there, in fact it's very bad in case of
       -- Input gates
       if (gate_op gate == Input && out == Just Blank)
         then return ()
         else MArr.writeArray vals (gate_num gate) (gate_f ins)
                  `trace` ("Evaluating gate number: " << (gate_num gate))



data GateVal = Blank |
               VScalar   Scalar |
               VArr      (Array.Array Integer (Maybe GateVal)) |
               VList [Maybe GateVal]  -- for output of a ReadDynArray
    deriving (Eq)






data Scalar = ScInt Integer |
              ScBool Bool |
              ScString String   -- this one only appears as the result of Print gates
    deriving (Eq)


-- helpers
vint = VScalar . ScInt



-- tristate logical operators - it was hard to make them ride on top of the builtin || and
-- && operators, because returning Nothing depends on the param values and not just their
-- Nothing/Just status.
triAnd, triOr  :: Maybe Bool -> Maybe Bool -> Maybe Bool
triAnd x y = case (x,y) of
               (Just False, _) -> Just False
               (_, Just False) -> Just False
               (Just True, Just True) -> Just True
               (_, _)           -> Nothing

triOr x y = case (x,y) of
              (Just True, _)            -> Just True
              (_, Just True)            -> Just True
              (Just False, Just False)  -> Just False
              (_, _)                    -> Nothing


type NodeFunc = ([Maybe GateVal] -> Maybe GateVal)

gate2func :: Gate -> NodeFunc
gate2func Gate { gate_op = op,
                 gate_num = num }
    = case op of
        Bin op
            -- these are a bit complicated as they may not need both parameters, ie. are
            -- well defined even if one of the params is Nothing
            | elem op [And, Or]
                            -> \[v1, v2]    -> let f = (case op of And  -> triAnd
                                                                   Or   -> triOr)
                                               in  do   b_out    <- f
                                                                    (do VScalar (ScBool b1) <-  v1
                                                                        return b1)
                                                                    (do VScalar (ScBool b2) <-  v2
                                                                        return b2)
                                                        return $ VScalar $ ScBool b_out

            | otherwise     -> \[v1, v2]    -> do VScalar s1 <- v1
                                                  VScalar s2 <- v2
                                                  ans <- binOpFunc op s1 s2
                                                  return $ VScalar ans
        
        Un  op              -> \[v]         -> do VScalar s <- v
                                                  return $ VScalar $ opUnOp op s

        Lit l               -> \[]          -> return $ VScalar $ case l of
                                                                    (LInt i)   -> ScInt i
                                                                    (LBool b)  -> ScBool b

        -- if the condition is Nothing, pass on the m_v_false
        Select              -> \[m_v_b,
                                 m_v_true,
                                 m_v_false] -> case m_v_b of
                                                 Nothing    -> m_v_false
                                                 Just v_b   -> let (VScalar (ScBool b)) = v_b
                                                               in
                                                                 case b of
                                                                   True   -> m_v_true
                                                                   False  -> m_v_false



        Slicer (Im.FieldLoc { Im.valloc = (off,len) })
                            -> \[args]      -> do VList m_vs    <- args
                                                  -- NOTE: if this pattern match fails (ie
                                                  -- there are too few elements in m_vs,
                                                  -- or len /= 1), result is Nothing
                                                  [ans]         <- sequence $
                                                                   take len . drop off $
                                                                   m_vs
                                                  return ans

        -- this puts in an arbitrary value of 0, don't have information on what the array
        -- type actually is, the proper writes should set that up.
        -- 
        -- actually Nothing sounds more sensible, as the values should be initialized
        -- before being used.
        --
        -- And furthermore, if the elements are structs, need more than one Nothing,
        -- wrapped in a VList
        InitDynArray elem_size len
                            -> \[]          -> let num_ints = elem_size `div` 5
                                                   val      = if num_ints == 1
                                                              then Nothing
                                                              else Just $
                                                                   VList $
                                                                   replicate num_ints Nothing
                                               in  return $
                                                   VArr $ listArray (0,len-1)
                                                                    (repeat val)

        WriteDynArray slice -> \(v_enable:
                                 v_arr:
                                 v_idx:
                                 v_vals)    -> do VScalar (ScBool en)   <- v_enable
                                                  VArr arr              <- v_arr
                                                  -- turn v_vals into a GateVal, using
                                                  -- VList if needed
                                                  -- For now, always
                                                  val                   <- return $ VList v_vals
                                                  let (off,len)         = Im.valloc slice
                                                  ( do VScalar (ScInt idx)   <- v_idx
                                                       let new_val = if (off,len) == (0,-1)
                                                                     then val
                                                                     -- elements must be
                                                                     -- lists if a slice is specified
                                                                     else let Just (VList vals) = arr ! idx
                                                                          in
                                                                            VList $ splice (off,len) v_vals vals
                                                       if not en ||
                                                          not (inRange (bounds arr) idx)
                                                        then fail ""
                                                        else return $ VArr $ arr // [(idx, Just new_val)] )
                                                     `catchError` -- if not enabled, v_idx is Nothing
                                                                  -- or out of range
                                                     ( const $ return $ VArr arr )

        ReadDynArray        -> \([v_enable,
                                  v_arr,
                                  v_idx])   -> do VScalar (ScBool en)   <- v_enable
                                                  VArr arr              <- v_arr
                                                  let ans_arr           =  Just $ VArr arr
                                                  -- if the index is Nothing or out of range, the value is
                                                  -- Nothing; likewise for enable = False
                                                  let val = (do VScalar (ScInt idx)   <- v_idx
                                                                val    <- if (not en ||
                                                                              not (inRange (bounds arr) idx))
                                                                            then fail ""
                                                                            else arr ! idx
                                                                return val
                                                            )
                                                      -- extract out of a VList
                                                      -- FIXME: rather awkward
                                                      vals = case val of Just (VList vs)    -> vs
                                                                         Nothing            -> []
                                                                         Just v             -> [Just v]
                                                  return $ VList (ans_arr:vals)

        Input               -> \[]          -> return Blank

        -- print just passes the values along. We get the printed values when its input
        -- values are shown by formatRun
        Print prompt        -> \v_xs        -> return $ VList v_xs
{-
        Print prompt        -> \v_xs        -> let x_strs = map (maybe "Nothing" show) v_xs
                                               in  Just $ VScalar $ ScString $
                                                   prompt ++
                                                   (concat . (List.intersperse ", ") $ x_strs)
  -}                                                
                                                                            
                                                                                   
                                  

binOpFunc o x y
    | (let ScInt i2 = y in i2 == 0 && elem o [Div, Mod])
                = Nothing       -- division or mod by zero
    | otherwise =
                  let opClass   = classifyBinOp o
                  in  Just $ case (opClass, x, y) of
                               (Arith,  ScInt i1, ScInt i2)     -> ScInt $ arithBinOp o i1 i2
                               (Binary, ScInt i1, ScInt i2)     -> ScInt $ arithBinOp o i1 i2
                               (Logical, ScBool b1, ScBool b2) -> ScBool $ boolBinOp o b1 b2
                               (Comparison, ScBool b1, ScBool b2)    -> ScBool $ compBinOp o b1 b2
                               (Comparison, ScInt i1, ScInt i2)      -> ScBool $ compBinOp o i1 i2


arithBinOp o = case o of
                              Times -> (*) 
                              Plus  -> (+) {- \x y -> x + y
                                    `trace`
                                     ("Adding " << x << " + " << y) -}
                              Div   -> div
                              Mod   -> mod
                              Minus -> (-) 
                              Max   -> max

                              SL     -> \x y -> x `shiftL` (fromIntegral y)
                              SR     -> \x y -> x `shiftR` (fromIntegral y)

                              BAnd   -> (.&.)
                              BOr    -> (.|.)
                              BXor   -> xor


boolBinOp o = case o of
                And -> (&&)
                Or  -> (||)
                
compBinOp o = case o of
              Lt         -> (<)
              Gt         -> (>) 
              LtEq       -> (<=)
              GtEq       -> (>=) 
              Eq         -> (==)
              Neq        -> (/=)

opUnOp :: UnOp -> (Scalar -> Scalar)
opUnOp o x = case o of
               Not  -> let (ScBool b) = x in ScBool $ not b
                    --                   BNot -> "~"
               Neg  -> let (ScInt i) = x in ScInt $ negate i
               Log2 -> let (ScInt i) = x in ScInt $ ilog2 i
             --                   Bitsize -> "bitsize"
{-
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
-}



-- NOTE: this should be kept synchronized with the LOG to 'gate_logger' in
-- run-circuit.cc.
showsValBinary :: Int -> Maybe GateVal -> ShowS
-- show a Nothing value as a zero byte followed by the bytes of a zero integer; would be
-- better to use "N" or such, but the evaluator would have a hard time picking Nothing
-- values out of a list.
showsValBinary p Nothing    = str "0" . str "," . (showsScalarBinary p $ ScInt 0)
showsValBinary p (Just g)   =
                    case g of
                            Blank     -> str "Blank"

                            -- a Just scalar needs an explicit '1' in front of it. For
                            -- arrays, it's understood as they should always be Just; For
                            -- Lists a Nothing value does not make sense, as they come out
                            -- of array gates so always have at least the array value
                            VScalar s -> str "1" . str "," . showsScalarBinary p s

                            -- the array contains (Maybe GateVal)'s.
                            -- Surround every array element with braces '{' and '}'
                            VArr arr  -> str "[" .
                                         (punctuate (str ",") $
                                          map (\val -> str "{" . rec val . str "}") $ elems arr) .
                                         str "]"

                            -- if a list contains several integers, they will be collapsed
                            -- into one comma-separated list of bytes, as the list
                            -- elements are not wrapped in brackets or such. This is to
                            -- match the evalutor---it would be difficult for the
                            -- evaluator to separate out list elements, so it just dumps a
                            -- list of bytes.
                            VList l   -> (punctuate (str ",") $ map rec l)

        where rec x      = showsValBinary p x


showsScalarBinary p s = case s of (ScInt i)     -> showsInt_bytes p i
                                  -- show boolean as the corresponding Int
                                  (ScBool b)    -> rec $ ScInt $ toInteger $ fromEnum b
                                  (ScString s)  -> showsPrec p s
    where rec = showsScalarBinary p



showsValHuman p Nothing = str "Nothing"
showsValHuman p (Just gv) =
                case gv of
                      Blank     -> str "Blank"
                      VScalar s -> rec s
                      VArr arr  -> str "array[" . rec (rangeSize $ bounds arr) . str "]"
                      VList l   -> str "vl " .
                                   str "[" .
                                           (punctuate (str ",") $ map (showsValHuman p) l) .
                                  str "]"
        where rec x = showsPrec p x -- not a recursion, but anyway.
              str   = (++)


showsScalarHuman p s = case s of (ScInt i)     -> showsPrec p i
                                 (ScBool b)    -> showsPrec p b
                                 (ScString s)  -> showsPrec p s




-- Show a Bits instance as a list of bytes
-- NOTE: setting bitSize to 32, as the standard bitSize function is not defined for
-- arbitrary-sized types like Integer.
showsInt_bytes p i = let bytes = map (i `extrByte`) [0..(bitSize i) `div` 8 - 1]
                     in  punctuate (str ",") $
                         map (showsPrec p) bytes
    where i `extrByte` b    = i `getBits` (b*8, b*8 + 7)
          bitSize _         = 32


-- make Show instances for GateVal and Scalar, for use with trace, logDebug, etc.
-- instance (Show (Maybe GateVal)) where
--    showsPrec = showsValHuman

instance (Show Scalar) where
    showsPrec = showsScalarHuman




-- this is useful in buidling ShowS instances - it appends a string.
str        = (++)


-- concatenate a list of ShowS with a separator.
punctuate :: ShowS -> [ShowS] -> ShowS
punctuate sep l = showsList $ intersperse sep $ l


-- concat a bunch of ShowS into a single one, without separators
showsList :: [ShowS] -> ShowS
showsList shows = foldl (.) id shows



-- ! Get a list of the indices of an MArray. Result in the monad, as the new API which is
-- needed here, getBounds, is in the monad, ie. there is no way to get the indices outside
-- the monad.
getMArrIndices :: (MArr.MArray a e m, Ix i) =>
                  a i e -> m [i]
getMArrIndices arr =
    do
#if __GLASGOW_HASKELL__ >= 605
     -- GHC version 6.5 and later
     bounds     <- MArr.getBounds arr
#else
     let bounds = MArr.bounds arr
#endif
     return $ Ix.range bounds


