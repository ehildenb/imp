IMP Language
============

Configuration
-------------

The IMP language has a `<k>` cell for execution and a `<mem>` cell for storage.
The `<k>` cell will store the remainder of the current program (the continuation), and the `<mem>` cell stores bindings to from variable names to values.

```k
module IMP
    imports DOMAINS

    configuration
      <imp>
        <k>   $PGM:Stmt </k>
        <mem> .Map      </mem>
      </imp>
```

Results
-------

Here we inform K that both sorts `Int` and `Bool` should be considered fully-evaluated results/values.
This affects the way that heating/cooling rules for the `strict` attribute are produced.

```k
    syntax KResult ::= Int | Bool
 // -----------------------------
```

Expressions
-----------

### Arithmetic Expressions

IMP has `AExp` for arithmetic expressions (over integers).
There is one special result `div-zero-error`, which division by zero leads to.
This error halts execution immediately (has no semantic rules).

```k
    syntax KItem ::= "div-zero-error"
 // ---------------------------------

    syntax AExp ::= Int | Id
                  | AExp "/" AExp [left, seqstrict]
                  | AExp "*" AExp [left, seqstrict]
                  > AExp "-" AExp [left, seqstrict]
                  | AExp "+" AExp [left, seqstrict]
                  | "(" AExp ")"  [bracket]
 // ---------------------------------------
    rule <k> I1 + I2 => I1 +Int I2     ... </k>
    rule <k> I1 - I2 => I1 -Int I2     ... </k>
    rule <k> I1 * I2 => I1 *Int I2     ... </k>
    rule <k>  I / 0  => div-zero-error ... </k>
    rule <k> I1 / I2 => I1 /Int I2     ... </k> requires I2 =/=Int 0
```

### Boolean Expressions

IMP has `BExp` for boolean expressions.

```k
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
    syntax Stmt ::= Id "=" AExp ";" [strict(2)]
 // -------------------------------------------
    rule <k> X:Id        => I ... </k> <mem> ... X |-> I        ... </mem>
    rule <k> X = I:Int ; => . ... </k> <mem> ... X |-> (_ => I) ... </mem>
```

Control Flow
------------

IMP has `if(_)_else_` for choice, `while(_)_` for looping, and `__` for sequencing statements.

```k
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

```k
    syntax priority int_;_IMP _=_;_IMP if(_)_else__IMP while(_)__IMP > ___IMP
endmodule
```
