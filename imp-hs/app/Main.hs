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
evalAExp ae@(ADiv   a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (/)
evalAExp ae@(AMul   a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (*)
evalAExp ae@(AMinus a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (-)
evalAExp ae@(APlus  a1 a2) m = maybe ae id   $ evalBinOp a1 a2 m (+)
evalAExp i _ = i

evalBinOp :: AExp -> AExp -> Memory -> (Int -> Int -> Int) -> Maybe AExp
evalBinOp a1 a2 m f = case (evalAExp a1 m, evalAExp a2 m) of
                        (AInt i1, AInt i2) -> Just $ AInt (f i1 i2)
                        _                  -> Nothing

evalBExp :: BExp -> Memory -> BExp
evalBExp (BBool b)    _ = b
evalBExp (BLE a1 a2)  m = (evalAExp a1 m) <= (evalAExp a2 m)
evalBExp (BLT a1 a2)  m = (evalAExp a1 m) <  (evalAExp a2 m)
evalBExp (BEQ a1 a2)  m = (evalAExp a1 m) == (evalAExp a2 m)
evalBExp (BNot b)     m = not (evalBExp b m)
evalBExp (BAnd b1 b2) m = (evalBExp b1 m) && (evalBExp b2 m)

main :: IO ()
main = pure ()
