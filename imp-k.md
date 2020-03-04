IMP Language
============

```k
module IMP
    imports BOOL
    imports INT
    imports MAP
    imports LIST
```

Configuration
-------------

The IMP language has a `<k>` cell for execution and a `<mem>` cell for storage.
The `<k>` cell will store the remainder of the current program (the continuation), and the `<mem>` cell stores bindings to from variable names to values.

```k
    configuration
      <imp>
        <k>   $PGM:Stmt </k>
        <mem> .Map      </mem>
      </imp>
```

Results
-------

Here we inform K that both sorts `Int` and `Bool` should be considered fully-evaluated results/values.

```k
    syntax Value ::= Int | Bool
 // ---------------------------
```

Expressions
-----------

### Evaluation

```k
    syntax Exp ::= AExp | BExp
 // --------------------------
```

### Arithmetic Expressions

IMP has `AExp` for arithmetic expressions (over integers).
There is one special result `div-zero-error`, which division by zero leads to.
This error halts execution immediately (has no semantic rules).

```k
    syntax AExp ::= Int | Id
                  | AExp "/" AExp [left]
                  | AExp "*" AExp [left]
                  > AExp "-" AExp [left]
                  | AExp "+" AExp [left]
                  | "(" AExp ")"  [bracket]
 // ---------------------------------------

    syntax Int ::= evalAExp ( AExp ) [function]
 // -------------------------------------------
    rule    evalAExp ( I:Int ) => I
    rule [[ evalAExp ( X:Id  ) => V ]] <mem> ... X |-> V ... </mem>

    rule evalAExp ( I1 + I2 ) => evalAExp ( I1 ) +Int evalAExp ( I2 )
    rule evalAExp ( I1 - I2 ) => evalAExp ( I1 ) -Int evalAExp ( I2 )
    rule evalAExp ( I1 * I2 ) => evalAExp ( I1 ) *Int evalAExp ( I2 )
    rule evalAExp ( I1 / I2 ) => evalAExp ( I1 ) /Int evalAExp ( I2 ) requires I2 =/=Int 0
```

### Boolean Expressions

IMP has `BExp` for boolean expressions.

```k
    syntax BExp ::= Bool
                  | AExp "<=" AExp
                  | AExp "<" AExp
                  | AExp "==" AExp
                  | "!" BExp
                  > BExp "&&" BExp [left]
                  | "(" BExp ")"   [bracket]
 // ----------------------------------------

    syntax Bool ::= evalBExp ( BExp ) [function]
 // --------------------------------------------
    rule evalBExp ( I1 <= I2    ) => evalAExp ( I1 ) <=Int evalAExp ( I2 )
    rule evalBExp ( I1 <  I2    ) => evalAExp ( I1 )  <Int evalAExp ( I2 )
    rule evalBExp ( I1 == I2    ) => evalAExp ( I1 ) ==Int evalAExp ( I2 )
    rule evalBExp ( ! B:Bool    ) => notBool evalBExp ( B )
    rule evalBExp ( B1    && B2 ) => evalBExp( evalBExp ( B1 ) && B2 ) requires notBool isBool(B1)
    rule evalBExp ( true  && B2 ) => evalBExp ( B2 )
    rule evalBExp ( false && B2 ) => false
```

Statements
----------

### Statement Blocks

IMP has `{_}` for creating blocks/groups of statements.
The braces are simple discarded in favor of their contents.

```k
    syntax Stmt ::= Block
 // ---------------------

    syntax Block ::= "{" "}" | "{" Stmt "}"
 // ---------------------------------------
    rule <k> {   } => . ... </k>
    rule <k> { S } => S ... </k>
```

### Variable Declaration/Assignment/Lookup

New variables can be declared with declaration `int_;` (and they are immediately initialized to 0).

```k
    syntax Ids ::= List{Id,","}
 // ---------------------------

    syntax Stmt ::= "int" Ids ";"
 // -----------------------------
    rule <k> int .Ids ; => . ... </k>
    rule <k> int (X, XS => XS) ; ... </k>
         <mem> MEM => MEM [ X <- 0 ] </mem>
```

Lookup and assignment (`_=_;`) read/affect the contents of the `<mem>` cell, for storing/retrieving values.

```k
    syntax Stmt ::= Id "=" AExp ";"
 // -------------------------------
    rule <k> X = ( AE => evalAExp(AE) ) ; => . ... </k> requires notBool isInt(AE)
    rule <k> X = I:Int ; => . ... </k>
         <mem> ... X |-> (_ => I) ... </mem>
```

Control Flow
------------

IMP has `if(_)_else_` for choice, `while(_)_` for looping, and `__` for sequencing statements.

```k
    syntax Stmt ::= "if" "(" BExp ")" Block "else" Block
 // ----------------------------------------------------
    rule <k> if ( C ) B1 else B2 => B1 ... </k> requires         evalBExp(C)
    rule <k> if ( C ) B1 else B2 => B2 ... </k> requires notBool evalBExp(C)

    syntax Stmt ::= "while" "(" BExp ")" Block
 // ------------------------------------------
    rule <k> while (B) STMT => if (B) {STMT while (B) STMT} else {} ... </k>

    syntax Stmt ::= Stmt Stmt [left]
 // --------------------------------
    rule <k> S1:Stmt S2:Stmt => S1 ~> S2 ... </k>
```

```k
    syntax priority int_;_IMP _=_;_IMP if(_)_else__IMP while(_)__IMP > ___IMP
```

```k
endmodule
```
