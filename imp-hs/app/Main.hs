module Main where

import Data.Attoparsec.ByteString
import Control.Monad
import Data.List
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM ( lookup , insert , empty , toList )

type Memory = HashMap Id Int

data Configuration = Imp { k   :: Pgm
                         , mem :: Memory
                         }

instance Show Configuration where
    show Imp { k = k , mem = m } = "<k> " ++ show k ++ " </k>\n<mem> " ++ fancyShow m ++ " </mem>"

fancyShow :: Memory -> String
fancyShow m = intercalate " " $ fmap fancyShow' (HM.toList m)
    where fancyShow' (k, v) = k ++ " |-> " ++ show v

data Pgm = Error PgmError Configuration
         | Pgm Stmt
         | Termin

instance Show Pgm where
    show (Error e c) = "Error " ++ show e ++ "    continuation: " ++ show c
    show (Pgm s)     = show s
    show Termin      = ""

data PgmError = EvalBExpError BExp
              | EvalAExpError AExp
              | ExecError Stmt

instance Show PgmError where
    show (EvalBExpError be) = "evaluating BExp: " ++ show be
    show (EvalAExpError ae) = "evaluating AExp: " ++ show ae
    show (ExecError s)      = "executing: " ++ show s

type Id = String

data Stmt = Skip
          | Block Stmt
          | VarDecls [ Id ]
          | Assign Id AExp
          | Conditional BExp Stmt Stmt
          | Loop BExp Stmt
          | Seq Stmt Stmt
    deriving Show

data AExp = AInt Int
          | AId  Id
          | ADiv   AExp AExp
          | AMul   AExp AExp
          | AMinus AExp AExp
          | APlus  AExp AExp
    deriving Show

data BExp = BBool Bool
          | BLE AExp AExp
          | BLT AExp AExp
          | BEQ AExp AExp
          | BNot BExp
          | BAnd BExp BExp
    deriving Show

evalAExp :: AExp -> Memory -> AExp
evalAExp ae@(AId aid)      m = maybe ae AInt $ HM.lookup aid m
evalAExp ae@(ADiv   a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (div) AInt
evalAExp ae@(AMul   a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (*)   AInt
evalAExp ae@(AMinus a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (-)   AInt
evalAExp ae@(APlus  a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (+)   AInt
evalAExp i _ = i

evalBExp :: BExp -> Memory -> BExp
evalBExp be@(BLE a1 a2) m = maybe be id $ (evalBinOp a1 a2 m) (<=) BBool
evalBExp be@(BLT a1 a2) m = maybe be id $ (evalBinOp a1 a2 m) (<)  BBool
evalBExp be@(BEQ a1 a2) m = maybe be id $ (evalBinOp a1 a2 m) (==) BBool
evalBExp be@(BAnd be1 be2) m = case (evalBExp be1 m, evalBExp be2 m) of
                                (BBool b1, BBool b2) -> BBool (b1 && b2)
                                _                    -> be
evalBExp (BNot be) m = case evalBExp be m of
                        BBool b -> BBool (not b)
                        _       -> BNot be
evalBExp b _ = b

evalBinOp :: AExp -> AExp -> Memory -> (Int -> Int -> a) -> (a -> b) -> Maybe b
evalBinOp a1 a2 m f w = case (evalAExp a1 m, evalAExp a2 m) of
                            (AInt i1, AInt i2) -> Just $ w (f i1 i2)
                            _                  -> Nothing

initVars :: [ Id ] -> Memory -> Memory
initVars []       m = m
initVars (v : vs) m = initVars vs $ HM.insert v 0 m

exec :: Configuration -> Configuration
exec Imp { k = Pgm (Skip)        , mem = m } = Imp { k = Termin , mem = m             }
exec Imp { k = Pgm (VarDecls vs) , mem = m } = Imp { k = Termin , mem = initVars vs m }

exec Imp { k = Pgm (Block s)     , mem = m } = Imp { k = Pgm s  , mem = m }

exec Imp { k = Pgm (Loop be s) , mem = m }
    = Imp { k = Pgm (Conditional be (Seq s (Loop be s)) Skip) , mem = m }

exec c@(Imp { k = Pgm (Conditional be s1 s2) , mem = m })
    = case evalBExp be m of
        BBool True  -> Imp { k = Pgm s1                      , mem = m }
        BBool False -> Imp { k = Pgm s2                      , mem = m }
        be'         -> Imp { k = Error (EvalBExpError be') c , mem = m }

exec c@(Imp { k = Pgm s@(Seq s1 s2), mem = m })
    = case exec Imp { k = Pgm s1 , mem = m } of
        Imp { k = Termin    , mem = m' } -> exec Imp { k = Pgm s2 , mem = m' }
        Imp { k = Error e _ , mem = m' } -> Imp { k = Error e c , mem = m' }
        Imp { k = Pgm s'    , mem = m' } -> Imp { k = Error (ExecError s') c , mem = m' }

exec c@(Imp { k = Pgm s@(Assign x ae) , mem = m })
    = case evalAExp ae m of
        AInt i -> Imp { k = Termin                      , mem = HM.insert x i m }
        ae'    -> Imp { k = Error (EvalAExpError ae') c , mem = m               }

inputPrograms :: [ Stmt ]
inputPrograms = [ VarDecls ["x" , "y"]
                , Seq (VarDecls ["x", "y"]) (Assign "x" (AInt 3))
                , Seq (Seq (VarDecls ["x", "y"]) (Assign "x" (AInt 3))) (Assign "y" (AMul (AInt 4) (AId "x")))
                ]

emptyConfig :: Stmt -> Configuration
emptyConfig s = Imp { k = Pgm s , mem = HM.empty }

main :: IO ()
main = putStrLn . intercalate "\n\n" . fmap (show . exec . emptyConfig) $ inputPrograms
