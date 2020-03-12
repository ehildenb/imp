module Main where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM ( lookup , insert , empty , toList )

-- Data
-- ================================================================================

type Memory = HashMap Id Int

type Id = String

data Configuration = Imp { k :: Pgm , mem :: Memory }
                   | Error PgmError Configuration

data Pgm = Pgm Stmt
         | Termin

data PgmError = EvalBExpError BExp
              | EvalAExpError AExp
              | ExecError Stmt

data Stmt = Skip
          | Block Stmt
          | VarDecls [ Id ]
          | Assign Id AExp
          | Conditional BExp Stmt Stmt
          | Loop BExp Stmt
          | Seq Stmt Stmt

data AExp = AInt Int
          | AId  Id
          | ADiv   AExp AExp
          | AMul   AExp AExp
          | AMinus AExp AExp
          | APlus  AExp AExp

data BExp = BBool Bool
          | BLE AExp AExp
          | BLT AExp AExp
          | BEQ AExp AExp
          | BNot BExp
          | BAnd BExp BExp

-- Prnting
-- ================================================================================

instance Show Configuration where
    show (Error e c) = "== Error " ++ show e ++ "\n    continuation:\n" ++ show c
    show Imp { k = k , mem = m } = "<k> " ++ show k ++ " </k>\n<mem> " ++ showMem m ++ " </mem>"
        where showMem m = intercalate " " $ fmap showMem' (HM.toList m)
              showMem' (k, v) = k ++ " |-> " ++ show v

instance Show Pgm where
    show (Pgm s) = show s
    show Termin  = ""

instance Show PgmError where
    show (EvalBExpError be) = "evaluating BExp: " ++ show be
    show (EvalAExpError ae) = "evaluating AExp: " ++ show ae
    show (ExecError s)      = "executing: " ++ show s

instance Show Stmt where
    show (Skip                ) = ""
    show (Block s             ) = "{ " ++ show s ++ " }"
    show (VarDecls ids        ) = "int " ++ (intercalate ", " ids) ++ ";"
    show (Assign x ae         ) = x ++ " = " ++ show ae ++ ";"
    show (Conditional be s1 s2) = "if (" ++ show be ++ ") { " ++ show s1 ++ " } else { " ++ show s2 ++ " }"
    show (Loop be s           ) = "while (" ++ show be ++ ") { " ++ show s ++ " }"
    show (Seq s1 s2           ) = show s1 ++ " " ++ show s2

instance Show AExp where
    show (AInt i      ) = show i
    show (AId x       ) = x
    show (ADiv   a1 a2) = show a1 ++ " / " ++ show a2
    show (AMul   a1 a2) = show a1 ++ " * " ++ show a2
    show (AMinus a1 a2) = show a1 ++ " - " ++ show a2
    show (APlus  a1 a2) = show a1 ++ " + " ++ show a2

instance Show BExp where
    show (BBool b  )  = show b
    show (BLE a1 a2)  = show a1 ++ " <= " ++ show a2
    show (BLT a1 a2)  = show a1 ++ " < "  ++ show a2
    show (BEQ a1 a2)  = show a1 ++ " == " ++ show a2
    show (BNot b)     = "! " ++ show b
    show (BAnd b1 b2) = show b1 ++ " && " ++ show b2

-- Parsing
-- ================================================================================

parseStmt :: Parser Stmt
parseStmt = error "parseStmt"

parseAExp :: Parser AExp
parseAExp = error "parseAExp"

parseBExp :: Parser BExp
parseBExp = error "parseBExp"

-- Expression Evaluation
-- ================================================================================

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

-- Program Execution
-- ================================================================================

initVars :: [ Id ] -> Memory -> Memory
initVars []       m = m
initVars (v : vs) m = initVars vs $ HM.insert v 0 m

exec :: Configuration -> Configuration
exec Imp { k = Pgm (Skip)        , mem = m } = Imp { k = Termin , mem = m             }
exec Imp { k = Pgm (VarDecls vs) , mem = m } = Imp { k = Termin , mem = initVars vs m }

exec Imp { k = Pgm (Block s) , mem = m } = exec Imp { k = Pgm s  , mem = m }

exec Imp { k = Pgm (Loop be s) , mem = m }
    = exec Imp { k = Pgm (Conditional be (Seq s (Loop be s)) Skip) , mem = m }

exec c@(Imp { k = Pgm (Conditional be s1 s2) , mem = m })
    = case evalBExp be m of
        BBool True  -> Imp { k = Pgm s1 , mem = m }
        BBool False -> Imp { k = Pgm s2 , mem = m }
        be'         -> Error (EvalBExpError be') c

exec c@(Imp { k = Pgm s@(Seq s1 s2), mem = m })
    = case exec Imp { k = Pgm s1 , mem = m } of
        Error e c'                    -> Error e c
        Imp { k = Termin , mem = m' } -> exec Imp { k = Pgm s2          , mem = m' }
        Imp { k = Pgm s' , mem = m' } -> exec Imp { k = Pgm (Seq s' s2) , mem = m' }

exec c@(Imp { k = Pgm s@(Assign x ae) , mem = m })
    = case evalAExp ae m of
        AInt i -> Imp { k = Termin , mem = HM.insert x i m }
        ae'    -> Error (EvalAExpError ae') c

-- Main Control Loop
-- ================================================================================

seqs :: [ Stmt ] -> Stmt
seqs []       = Skip
seqs (s : ss) = Seq s (seqs ss)

ints :: [ Id ] -> Stmt
ints is = VarDecls is

inputPrograms :: [ Stmt ]
inputPrograms = [ VarDecls ["x" , "y"]
                , seqs [ ints ["x", "y"]
                       , Assign "x" (AInt 3)
                       ]
                , seqs [ ints ["x", "y"]
                       , Assign "x" (AInt 3), Assign "y" (AMul (AInt 4) (AId "x"))
                       ]
                , seqs [ ints ["x", "y"]
                       , Assign "x" (AInt 3), Assign "y" (AMul (AInt 4) (AId "z"))
                       ]
                , seqs [ ints ["s", "n"]
                       , Assign "n" (AInt 10)
                       , Loop (BNot (BLE (AId "n") (AInt 0))) (seqs [ Assign "s" (APlus (AId "s") (AId "n")) , Assign "n" (AMinus (AId "n") (AInt 1)) ])
                       ]
                ]

emptyConfig :: Stmt -> Configuration
emptyConfig s = Imp { k = Pgm s , mem = HM.empty }

main :: IO ()
main = putStrLn . intercalate "\n\n" . fmap (show . exec . emptyConfig) $ inputPrograms
