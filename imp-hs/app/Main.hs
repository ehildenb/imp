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
evalAExp ae@(ADiv   a1 a2) m = maybe ae id   $ evalBinAExp a1 a2 m (div)
evalAExp ae@(AMul   a1 a2) m = maybe ae id   $ evalBinAExp a1 a2 m (*)
evalAExp ae@(AMinus a1 a2) m = maybe ae id   $ evalBinAExp a1 a2 m (-)
evalAExp ae@(APlus  a1 a2) m = maybe ae id   $ evalBinAExp a1 a2 m (+)
evalAExp i _ = i

evalBinAExp :: AExp -> AExp -> Memory -> (Int -> Int -> Int) -> Maybe AExp
evalBinAExp a1 a2 m f = case (evalAExp a1 m, evalAExp a2 m) of
                            (AInt i1, AInt i2) -> Just $ AInt (f i1 i2)
                            _                  -> Nothing

evalBExp :: BExp -> Memory -> BExp
evalBExp be@(BLE a1 a2) m = maybe be id $ (evalCompBExp a1 a2 m) (<=)
evalBExp be@(BLT a1 a2) m = maybe be id $ (evalCompBExp a1 a2 m) (<)
evalBExp be@(BEQ a1 a2) m = maybe be id $ (evalCompBExp a1 a2 m) (==)
evalBExp be@(BAnd be1 be2) m = case (evalBExp be1 m, evalBExp be2 m) of
                                (BBool b1, BBool b2) -> BBool (b1 && b2)
                                _                    -> be
evalBExp (BNot be) m = case evalBExp be m of
                        BBool b -> BBool (not b)
                        _       -> BNot be
evalBExp b _ = b

evalCompBExp :: AExp -> AExp -> Memory -> (Int -> Int -> Bool) -> Maybe BExp
evalCompBExp ae1 ae2 m f = case (evalAExp ae1 m, evalAExp ae2 m) of
                            (AInt a1, AInt a2) -> Just $ BBool (f a1 a2)
                            _                  -> Nothing

main :: IO ()
main = pure ()
