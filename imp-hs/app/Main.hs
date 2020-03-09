module Main where

import Data.Attoparsec.ByteString
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM ( lookup )

type Memory = HashMap Id Int

data Configuration = Imp { k   :: Pgm
                         , mem :: Memory
                         }

data Result = I Int | B Bool

data Pgm = Error PgmError
         | Pgm Stmt

data PgmError = DivZeroError

type Id = String

data Stmt = Block Stmt
          | VarDecls [ Id ]
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

main :: IO ()
main = pure ()
