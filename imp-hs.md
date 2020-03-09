IMP Language
============

```hs
module Lang.Imp

import Attoparsec
import HashMap
```

Configuration
-------------

The IMP language has a `<k>` cell for execution and a `<mem>` cell for storage.
The `<k>` cell will store the remainder of the current program (the continuation), and the `<mem>` cell stores bindings to from variable names to values.

```hs
type Memory = Map

data Configuration = Imp { k   :: Pgm
                         , mem :: Memory
                         }
```

Results
-------

Here we inform K that both sorts `Int` and `Bool` should be considered fully-evaluated results/values.
This affects the way that heating/cooling rules for the `strict` attribute are produced.

```hs
data Result = I Int | B Bool
```

Expressions
-----------

### Arithmetic Expressions

IMP has `AExp` for arithmetic expressions (over integers).
There is one special result `div-zero-error`, which division by zero leads to.
This error halts execution immediately (has no semantic rules).

```hs
data Pgm = Error PgmError
         | Pgm Stmt

data PgmError = DivZeroError

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

evalAExp :: AExp -> Memory -> Int
evalAExp (AInt i)  _      = i
evalAExp (AId  id) m      = m [ id ]
evalAExp (ADiv   a1 a2) m = (evalAExp a1 m) / (evalAExp a2 m)
evalAExp (AMul   a1 a2) m = (evalAExp a1 m) * (evalAExp a2 m)
evalAExp (AMinus a1 a2) m = (evalAExp a1 m) - (evalAExp a2 m)
evalAExp (APlus  a1 a2) m = (evalAExp a1 m) + (evalAExp a2 m)

evalBExp :: BExp -> Memory -> Bool
evalBExp (BBool b)    _ = b
evalBExp (BLE a1 a2)  m = (evalAExp a1 m) <= (evalAExp a2 m)
evalBExp (BLT a1 a2)  m = (evalAExp a1 m) <  (evalAExp a2 m)
evalBExp (BEQ a1 a2)  m = (evalAExp a1 m) == (evalAExp a2 m)
evalBExp (BNot b)     m = ! (evalBExp b m)
evalBExp (BAnd b1 b2) m = (evalBExp b1 m) && (evalBExp b2 m)
```

### Boolean Expressions

IMP has `BExp` for boolean expressions.

```hs
    syntax BExp ::= Bool
                  | AExp "<=" AExp [seqstrict]
                  | AExp "<" AExp  [seqstrict]
                  | AExp "==" AExp [seqstrict]
                  | "!" BExp       [strict]
                  > BExp "&&" BExp [left, strict(1)]
                  | "(" BExp ")"   [bracket]
 // ----------------------------------------
    rule <k> I1 <= I2   => I1 <=Int I2 ... </k>
    rule <k> I1 <  I2   => I1 <Int  I2 ... </k>
    rule <k> I1 == I2   => I1 ==Int I2 ... </k>
    rule <k> ! T:Bool   => notBool T   ... </k>
    rule <k> true  && B => B           ... </k>
    rule <k> false && _ => false       ... </k>
```

Statements
----------

### Statement Blocks

IMP has `{_}` for creating blocks/groups of statements.
The braces are simple discarded in favor of their contents.

```hs
    syntax Stmt ::= Block
 // ---------------------

    syntax Block ::= "{" "}" | "{" Stmt "}"
 // ---------------------------------------
    rule <k> {   } => . ... </k>
    rule <k> { S } => S ... </k>
```

### Variable Declaration/Assignment/Lookup

New variables can be declared with declaration `int_;` (and they are immediately initialized to 0).

```hs
    syntax Ids ::= List{Id,","}
 // ---------------------------

    syntax Stmt ::= "int" Ids ";"
 // -----------------------------
    rule <k> int .Ids ; => . ... </k>
    rule <k> int (X, XS => XS) ; ... </k>
         <mem> MEM => MEM [ X <- 0 ] </mem>
```

Lookup and assignment (`_=_;`) read/affect the contents of the `<mem>` cell, for storing/retrieving values.

```hs
    syntax Stmt ::= Id "=" AExp ";" [strict(2)]
 // -------------------------------------------
    rule <k> X:Id        => I ... </k> <mem> ... X |-> I        ... </mem>
    rule <k> X = I:Int ; => . ... </k> <mem> ... X |-> (_ => I) ... </mem>
```

Control Flow
------------

IMP has `if(_)_else_` for choice, `while(_)_` for looping, and `__` for sequencing statements.

```hs
    syntax Stmt ::= "if" "(" BExp ")" Block "else" Block [strict(1)]
 // ----------------------------------------------------------------
    rule <k> if (true)  B1 else _  => B1 ... </k>
    rule <k> if (false) _  else B2 => B2 ... </k>

    syntax Stmt ::= "while" "(" BExp ")" Block
 // ------------------------------------------
    rule <k> while (B) STMT => if (B) {STMT while (B) STMT} else {} ... </k>

    syntax Stmt ::= Stmt Stmt [left]
 // --------------------------------
    rule <k> S1:Stmt S2:Stmt => S1 ~> S2 ... </k>
```

```hs
    syntax priority int_;_IMP _=_;_IMP if(_)_else__IMP while(_)__IMP > ___IMP
```

```hs
endmodule
```
