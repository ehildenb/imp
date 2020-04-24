Abstract
========

This is the K semantic definition of the untyped FUN language.
FUN is a pedagogical and research language that captures the essence of the functional programming paradigm, extended with several features often encountered in functional programming languages.
Like many functional languages, FUN is an expression language, that is, everything, including the main program, is an expression.
Functions can be declared anywhere and are first class values in the language.
FUN is call-by-value here, but it has been extended (as student homework assignments) with other parameter-passing styles.
To make it more interesting and to highlight some of K's strengths, FUN includes the following features:

-   The basic builtin data-types of integers, booleans and strings.

-   Builtin lists, which can hold any elements, including other lists.
    Lists are enclosed in square brackets and their elements are colon-separated; e.g., `[ 1 : 2 : 3 : .Vals ]`.

-   User-defined data-types, by means of constructor terms.
    Constructor names start with a capital letter (while any other identifier in the language starts with a lowercase letter), and they can be followed by an arbitrary number of space separated arguments.
    For example, `Pair 5 7` is a constructor term holding two numbers, `Cons 1 (Cons 2 (Cons 3 Nil))` is a list-like constructor term holding 3 elements, and `Tree (Tree (Leaf 1) (Leaf 2)) (Leaf 3)` is a tree-like constructor term holding 3 elements.
    In the untyped version of the FUN language, no type checking or inference is performed to ensure that the data constructors are used correctly.
    The execution will simply get stuck when they are misused.
    Moreover, since no type checking is performed, the data-types are not even declared in the untyped version of FUN.

-   Functions declared in `let`/`letrec` binders and declared anonymously can take multiple space-separated arguments.
    In addition, functions can be partially applied to arguments, allowing the function to be evaluated incrementally.

-   Functions can be defined using pattern matching over the available declared data-types.

-   We include a `callcc` construct ("call with current continuation") to demonstrate the modularity of K semantic definitions.
    This allows for using `try`/`catch` control flow constructs.

Syntax
======

```k
module FUN-COMMON
    imports MAP
    imports INT
    imports FLOAT
    imports STRING
```

```kcompile
    imports STRATEGY
```

FUN is an expression language.
The constructs below fall into several categories: names, arithmetic constructs, conventional functional constructs, patterns and pattern matching, data constructs, lists, and call-with-current-continuation (callcc).
The arithmetic constructs are standard; they are present in almost all our K language definitions.
The meaning of FUN's constructs are discussed in more depth when we define their semantics in the next module.

The Syntactic Constructs
------------------------

We start with the syntactic definition of FUN names.
We have several categories of names: ones to be used for functions and variables, others to be used for data constructors, others for types and others for type variables.
We will introduce them as needed, starting with the former category.
We prefer the names of variables and functions to start with lower case letters.

```k
    syntax Name  ::= "$x" | "$k"
    syntax Names ::= List{Name,","}
 // -------------------------------
```

### Values

Expression constructs will be defined throughtout the syntax module.
Below are the very basic ones, namely the builtins, the names, and the parentheses used as brackets for grouping.
Lists of expressions are declared strict, so all expressions in the list get evaluated whenever the list is on a position which can be evaluated:

```k
    syntax KResult ::= Val | Vals
 // -----------------------------

    syntax ConstructorName
    syntax ConstructorVal ::= ConstructorName
                            | ConstructorVal Val [left, klabel(applyConstructor), symbol, prefer]
 // ---------------------------------------------------------------------------------------------

    syntax Val ::= Int | Bool | String | ConstructorVal | val ( Exp )
 // -----------------------------------------------------------------

    syntax Exp ::= Val | Name
                 | "(" Exp ")" [bracket]
 // ------------------------------------
```

```k
    syntax Exp ::= Exp Exp [left, klabel(applyExp), symbol, avoid]
 // --------------------------------------------------------------

    syntax Bool ::= isApplication ( Exp ) [function]
 // ------------------------------------------------
    rule isApplication(_:Exp            _:Exp) => true
    rule isApplication(_:ConstructorVal _:Val) => true
    rule isApplication(_)                      => false [owise]
```

### Builtin Lists

FUN's builtin lists are `_:_` separated cons-lists like many functional languages support.
A list is turned back into a regular element by wrapping it in the `[_]` operator.

```k
    syntax Vals ::= ".Vals" | Val | Val ":" Vals [klabel(valCons), symbol, prefer]
    syntax Exps ::= Vals    | Exp | Exp ":" Exps [klabel(expCons), symbol]
 // ----------------------------------------------------------------------

    syntax Val ::= "[" Vals "]" [klabel(valList), symbol, prefer]
    syntax Exp ::= "[" Exps "]" [klabel(expList), symbol]
 // -----------------------------------------------------

    syntax Val ::= "[" "]"
 // ----------------------
    rule [ ] => [ .Vals ] [macro]
```

### Expressions

We next define the syntax of arithmetic constructs, together with their relative priorities and left-/non-associativities.
We also tag all these rules with a new tag, "arith", so we can more easily define global syntax priorities later (at the end of the syntax module).

**TODO**: Left attribute on `_^_` and `_+_` should not be necessary; currently a parsing bug.
          The "prefer" attribute above is to not parse x-1 as x(-1).
          Due to some parsing problems, we currently cannot add a unary minus (`3 + - 3`).

```k
    syntax Exp ::= left:
                   Exp "*" Exp    [seqstrict, arith]
                 | Exp "/" Exp    [seqstrict, arith]
                 | Exp "%" Exp    [seqstrict, arith]
                 > left:
                   Exp "+" Exp    [seqstrict, left, arith]
                 | Exp "^" Exp    [seqstrict, left, arith]
                 | Exp "-" Exp    [seqstrict, prefer, arith]
                 | "-" Exp        [seqstrict, arith]
                 > non-assoc:
                   Exp "<"  Exp   [seqstrict, arith]
                 | Exp "<=" Exp   [seqstrict, arith]
                 | Exp ">"  Exp   [seqstrict, arith]
                 | Exp ">=" Exp   [seqstrict, arith]
                 | Exp "==" Exp   [seqstrict, arith]
                 | Exp "!=" Exp   [seqstrict, arith]
                 > "!" Exp        [seqstrict, arith]
                 > Exp "&&" Exp   [strict(1), left, arith]
                 > Exp "||" Exp   [strict(1), left, arith]
 // ------------------------------------------------------
```

The conditional construct has the expected evaluation strategy, stating that only the first argument always evaluated:

```k
    syntax Exp ::= "if" Exp "then" Exp "else" Exp  [strict(1), klabel(ite), symbol]
 // -------------------------------------------------------------------------------
```

### Algebraic Data Types

FUN also allows polymorphic datatype declarations.
These will be useful when we define the type system later on.

**NOTE**: In a future version of K, we want the datatype declaration to be a construct by itself, but that is not possible currently because K's parser wronly identifies the BLAH operation allowing a declaration to appear in front of an expression with the function application construct, giving ambiguous parsing errors.

```k
    syntax Exp ::= "datatype" Type "=" TypeCases Exp [klabel(data), symbol]
 // -----------------------------------------------------------------------
    rule datatype T = TCS E => E [macro]
```

Data constructors start with capital letters and they may or may not have arguments.
We need to use the attribute "prefer" to make sure that, e.g., `Cons(a)` parses as constructor `Cons` with argument `a`, and not as the expression `Cons` applied (as a function) to argument `a`.
Also, note that the constructor is strict in its second argument, because we want to evaluate its arguments but not the constuctor name itsef.

### (Lambda) Functions

A function is essentially a "`|`"-separated ordered sequence of cases, each case of the form "`pattern -> expression`", preceded by the language construct `fun`.
Patterns will be defined shortly, both for the builtin lists and for user-defined constructors.
Recall that the syntax we define in K is not meant to serve as a ultimate parser for the defined language, but rather as a convenient notation for K abstract syntax trees, which we prefer when we write the semantic rules.
It is therefore often the case that we define a more "generous" syntax than we want to allow programs to use.

Specifically, the syntax of `Cases` below allows any expressions to appear as pattern.
This syntactic relaxation permits many wrong programs to be parsed, but that is not a problem because we are not going to give semantics to wrong combinations, so those programs will get stuck; moreover, our type inferencer will reject those programs anyway.
Function application is just concatenation of expressions, without worrying about type correctness.
Again, the type system will reject type-incorrect programs.

```k
    syntax Exp ::= "fun" Cases [klabel(fun), symbol]
 // ------------------------------------------------

    syntax Case ::= "->" Exp
                  | Exp Case [klabel(casePattern), symbol]
 // ------------------------------------------------------

    syntax Cases ::= List{Case, "|"}
 // --------------------------------
```

### Binding Environments

The `let` and `letrec` binders have the usual syntax and functional meaning.
We allow multiple (potentially recursive) and-separated bindings.

**TODO**: The "prefer" attribute for letrec currently needed due to tool bug, to make sure that "letrec" is not parsed as "let rec".

```k
    syntax Exp ::= "let"    Bindings "in" Exp [klabel(let), symbol]
                 | "letrec" Bindings "in" Exp [prefer, klabel(letrec), symbol]
 // --------------------------------------------------------------------------
```

Bindings themselves comprise of expressions `E = E'`, where variables in `E` are bound by matching before evaluating `E'`.

```k
    syntax Binding  ::= Exp "=" Exp
    syntax Bindings ::= List{Binding,"and"}
 // ---------------------------------------

    syntax Name  ::= #name  ( Binding  ) [function]
    syntax Names ::= #names ( Bindings ) [function]
 // -----------------------------------------------
    rule #names(.Bindings)        => .Names
    rule #names(B:Binding and BS) => #name(B) , #names(BS)

    rule #name(N:Name = _) => N
    rule #name((E:Exp E':Exp => E) = _)

    syntax Exp  ::= #exp  ( Binding  ) [function]
    syntax Exps ::= #exps ( Bindings ) [function]
 // ---------------------------------------------
    rule #exps(.Bindings)        => .Vals
    rule #exps(B:Binding and BS) => #exp(B) : #exps(BS)

    rule #exp(_:Name = E) => E
    rule #exp(E:Exp E':Exp = fun C:Case => E = fun E' C     )
    rule #exp(E:Exp E':Exp = E''        => E = fun E' -> E'') [owise]
```

### Jumps in Control Flow

Call-with-current-continuation, named `callcc` in FUN, is a powerful control operator that originated in the Scheme programming language, but it now exists in many other functional languages.
It works by evaluating its argument, expected to evaluate to a function, and by passing the current continuation, or evaluation context (or computation, in K terminology), as a special value to it.
When/If this special value is invoked, the current context is discarded and replaced with the one held by the special value and the computation continues from there.
It is like taking a snapshot of the execution context at some moment in time and then, when desired, being able to get back in time to that point.
If you like games, it is like saving the game now (so you can work on your homework!) and then continuing the game tomorrow or whenever you wish.
To issustrate the strength of `callcc`, we also allow exceptions in FUN by means of a conventional `try-catch` construct, which will desugar to `callcc`.
We also need to introduce the special expression contant `throw`, but we need to use it as a function argument name in the desugaring macro, so we define it as a name instead of as an expression constant:

```k
    syntax Name ::= "throw" [token]
    syntax Exp  ::= "callcc"
                  | "try" Exp "catch" "(" Name ")" Exp
 // --------------------------------------------------
    rule try E catch(X) E' => callcc (fun $k -> (fun throw -> E) (fun X -> $k E')) [macro]
```

### Types

We next need to define the syntax of types and type cases that appear in datatype declarations.
Like in many functional languages, type parameters/variables in user-defined types are quoted identifiers.

```k
    syntax TypeVar
    syntax TypeVars ::= List{TypeVar,","} [prefer]
 // ----------------------------------------------
```

Types can be basic types, function types, or user-defined parametric types.
In the dynamic semantics we are going to simply ignore all the type declations, so here the syntax of types below is only useful for generating the desired parser.
To avoid syntactic ambiguities with the arrow construct for function cases, we use the symbol `-->` as a constructor for function types:

```k
    syntax TypeName
    syntax Type ::= "int" | "bool" | "string"
                  | Type "-->" Type           [right]
                  | "(" Type ")"              [bracket]
                  | TypeVar
                  | TypeName                  [klabel(TypeName), avoid]
                  | Type TypeName             [klabel(Type-TypeName)]
                  | "(" Types ")" TypeName    [prefer]
 // --------------------------------------------------
    rule T:Type TN:TypeName => (T) TN [macro-rec]

    syntax Types ::= List{Type,","}
    syntax Types ::= TypeVars
 // -------------------------

    syntax TypeCase  ::= ConstructorName
                       | TypeCase Type
    syntax TypeCases ::= List{TypeCase,"|"} [klabel(_|TypeCase_)]
 // -------------------------------------------------------------
```

Additional Priorities
---------------------

These inform the parser of precedence information when ambiguous parses show up.

```k
    syntax priorities casePattern
                    > applyConstructor applyExp
                    > arith
                    > let letrec ite
                    > fun
                    > data
endmodule
```

FUN Identifier Instantiation
----------------------------

The following module instantiates the empty identifier sorts declared above with their corresponding regular expressions.

```k
module FUN-SYNTAX
    imports FUN-COMMON
    imports BUILTIN-ID-TOKENS

    syntax Name            ::= r"[a-z][_a-zA-Z0-9]*"      [autoReject, token, prec(2)]
                             | #LowerId                   [autoReject, token]
    syntax ConstructorName ::= #UpperId                   [autoReject, token]
    syntax TypeVar         ::= r"['][a-z][_a-zA-Z0-9]*"   [autoReject, token]
    syntax TypeName        ::= Name                       [autoReject, token]
endmodule
```

Semantics
=========

The semantics below is environment-based.

```k
module FUN
    imports FUN-COMMON
    imports LIST
```

Configuration
-------------

Computation will happen on the `<k>` cell.
The `<store>` will be used as a memory, which is addressed through the `<env>`.
`<nextLoc>` tracks the next available `<store>` location, and is incremented on allocation.

```k
    configuration
      <FUN>
        <k>    $PGM:Exp </k>
        <env>  .Map     </env>
        <envs> .List    </envs>
      </FUN>
```

Lookup
------

```k
    rule <k> X:Name => V ... </k>
         <env> ... X |-> V ... </env>
```

Expressions
-----------

```k
    rule <k> I1 * I2 => I1 *Int I2 ... </k>
    rule <k> I1 / I2 => I1 /Int I2 ... </k> requires I2 =/=K 0
    rule <k> I1 % I2 => I1 %Int I2 ... </k> requires I2 =/=K 0
    rule <k> I1 + I2 => I1 +Int I2 ... </k>
    rule <k> I1 - I2 => I1 -Int I2 ... </k>
    rule <k>    - I  => 0  -Int I  ... </k>

    rule <k> S1 ^ S2 => S1 +String S2 ... </k>

    rule <k> I1 <  I2 => I1  <Int I2 ... </k>
    rule <k> I1 <= I2 => I1 <=Int I2 ... </k>
    rule <k> I1 >  I2 => I1  >Int I2 ... </k>
    rule <k> I1 >= I2 => I1 >=Int I2 ... </k>

    rule <k> V1:Val == V2:Val => V1  ==K V2 ... </k>
    rule <k> V1:Val != V2:Val => V1 =/=K V2 ... </k>

    rule <k>        ! T:Bool => notBool(T) ... </k>
    rule <k> true  && E      => E          ... </k>
    rule <k> false && _      => false      ... </k>
    rule <k> true  || _      => true       ... </k>
    rule <k> false || E      => E          ... </k>
```

```k
    rule <k> [ ES:Exps ] => ES ~> [ ] ... </k>
    rule <k> VS:Vals ~> [ ] => [ VS ] ... </k>

    syntax KItem ::= "#consHead" Val | "#consTail" Exps
 // ---------------------------------------------------
    rule <k> E:Exp : ES             => E ~> #consTail ES ... </k>
    rule <k> V:Val ~> #consTail ES  => ES ~> #consHead V ... </k>
    rule <k> VS:Vals ~> #consHead V => V : VS            ... </k>
```

Conditional
-----------

Because the conditional is declared `strict`, we only need to provide the semantics in terms of values.

```k
    rule <k> if  true then E else _ => E ... </k>
    rule <k> if false then _ else E => E ... </k>
```

Functions and Closures
----------------------

Like in the environment-based semantics of LAMBDA++ in the first part of the K tutorial, functions evaluate to closures.
A closure includes the an environment and the function contents.
The environment will be used at execution time to lookup non-parameter variables that appear free in the function body.

```k
    syntax Exp ::= closure ( Map , Cases , Bindings , Vals ) [klabel(partialClosure), symbol]
 // -----------------------------------------------------------------------------------------
    rule <k> fun CASES => closure(RHO, CASES, .Bindings, .Vals) ... </k>
         <env> RHO </env>
```

In evaluating an application, the arguments are evaluated in reverse order until we reach the applied function.

```k
    syntax Arg   ::= #arg   ( Val )
    syntax KItem ::= #apply ( Exp )
 // -------------------------------
    rule <k> applyExp(E:Exp, V)  => E ~> #arg(V)    ... </k>
    rule <k> applyExp(E, E':Exp) => E' ~> #apply(E) ... </k> requires notBool isVal(E')

    rule <k> CV:ConstructorVal ~> #arg(V) => CV V ... </k>

    rule <k> V:Val ~> #apply(E) => E V ... </k>
```

Finally, once all arguments are evaluated, we can attempt pattern matching on the closure's function contents.
The arguments are collected, and an empty substitution is initialized in a `matchResult`.
As each argument is consumed, we match it against the closure's next pattern, incrementally building up the substitution.
On failure, the process is restarted on the next `Case` in the closure function contents.

```k
    rule <k> (val(closure(_, _, _, _) #as CL) => CL) ~> #arg(_) ... </k>

    rule <k> (. => getMatchingBindings(P, V, BS)) ~> closure(RHO, (P C => C) | CS, BS => .Bindings, VS => V : VS) ~> (#arg(V) => .) ... </k>

    rule <k> (matchResult(BS) => .) ~> closure(RHO ,  _                  , _  => BS , VS          )                             ... </k>
    rule <k> (matchFailure    => .) ~> closure(RHO , (C:Case | CS => CS) , BS       , VS => .Vals ) ~> (. => #sequenceArgs(VS)) ... </k>

    rule <k> closure(RHO, -> E | _, BS, _) => pushEnv ~> setEnv(RHO) ~> let BS in E ~> popEnv ... </k>

    rule <k> closure(_, _, _, _) #as CL => val(CL) ... </k> [owise]

    syntax K ::= #sequenceArgs ( Vals ) [function]
 // ----------------------------------------------
    rule #sequenceArgs(.Vals)  => .
    rule #sequenceArgs(V : VS) => #sequenceArgs(VS) ~> #arg(V)
```

Let and Letrec
--------------

The constructs `let` and `letrec` are very similar, but treat bound closures in the environment differently.
Closures defined in `let` bindings must only contain the environment of the `let` binding, while `letrec` closures must also contain the current bindings in their environment.
Helpers `binds` and `bindsRec` ensure that the definitions are evaluated in the correct environment, before and after the bindings are allocated respectively.

```k
    rule <k> let    BS in E => pushEnv ~> #exps(BS) ~> #assign  (#names(BS)) ~> E ~> popEnv ... </k>
    rule <k> letrec BS in E => pushEnv ~> #exps(BS) ~> #assignMu(#names(BS)) ~> E ~> popEnv ... </k>

    syntax Exp ::= mu ( Names , Exp )
 // ---------------------------------
    rule <k> mu ( .Names , E ) => E    ... </k>

    rule <k> mu ( (X , XS => XS) , E ) ... </k>
         <env> ENV => ENV[X <- ENV'[X]] </env>
         <envs> ListItem(_) ListItem(ENV':Map) ... </envs>
      requires X in_keys(ENV')
```

The following helpers actually do the allocation and assignment operations on the storage.

```k
    syntax KItem ::= #assign   ( Names )
                   | #assignMu ( Names )
 // ------------------------------------
    rule <k> VS:Vals ~> (#assignMu(XS) => #markMus(XS) ~> #assign(XS)) ... </k>

    rule <k> .Vals ~> #assign(.Names) => . ... </k>

    rule <k> (#listTailMatch(V) : VS:Vals => VS) ~> #assign(X , XS => XS) ... </k>
         <env> ENV => ENV[X <- V] </env>

    rule <k> (V:Val : VS:Vals => VS) ~> #assign(X , XS => XS) ... </k>
         <env> ENV => ENV[X <- V] </env>
      [owise]
```

This machinery actually ensures that the recursive expressions know which values they may access from the enclosing environment.

```k
    syntax KItem ::= #markMus ( Names )
 // -----------------------------------
    rule <k> VS:Vals ~> #markMus(XS) => #applyMuVals(XS, VS) ... </k>

    syntax Vals  ::= #applyMuVals ( Names , Vals ) [function]
    syntax Val   ::= #applyMuVal  ( Names , Val  ) [function]
 // ---------------------------------------------------------
    rule #applyMuVals(XS, .Vals)  => .Vals
    rule #applyMuVals(XS, V : VS) => #applyMuVal(XS, V) : #applyMuVals(XS, VS)

    rule #applyMuVal(XS, val(closure(RHO, CS, BS, VS))) => val(closure(RHO, #applyMuCases(XS, CS), BS, VS))
    rule #applyMuVal(XS, V)                             => V [owise]

    syntax Cases ::= #applyMuCases ( Names , Cases ) [function]
    syntax Case  ::= #applyMuCase  ( Names , Case  ) [function]
 // -----------------------------------------------------------
    rule #applyMuCases(XS, .Cases) => .Cases
    rule #applyMuCases(XS, C | CS) => #applyMuCase(XS, C) | #applyMuCases(XS, CS)

    rule #applyMuCase(XS, P C)  => P #applyMuCase(XS, C)
    rule #applyMuCase(XS, -> E) => -> mu(XS, E)
```

Callcc
------

As we know it from the LAMBDA++ tutorial, call-with-current-continuation is quite easy to define in K.
We create a wrapper `cc(RHO, K)` which stores the current environment `RHO` and remainder of the computation `K`.
`callcc E` evaluates `E` and calls the resulting closure with argument `cc(RHO, K)`.
If the resulting closure invokes the stored `cc(RHO, K)`, the current state is replaced with the stored environment `RHO` and computation `K`.

```k
    syntax Val ::= cc ( Map , K )
 // -----------------------------
    rule isVal(callcc) => true

    rule <k> (callcc ~> #arg(V) => V cc(RHO, K)) ~> K </k>
         <env> RHO </env>

    rule <k> cc(RHO, K) ~> #arg(V) ~> _ => setEnv(RHO) ~> V ~> K </k>

    rule <k> val(closure(RHO, CS, BS, VS)) cc(RHO', K) => val(closure(RHO, CS, BS, VS)) ~> #arg(cc(RHO', K)) ... </k>
```

Auxiliary operations
--------------------

### Environment recovery

Environment recovery is used in multiple places where a sub-expression needs to be evaluated in a different environment than the current one.
`setEnv(RHO)` is used to recover the original environment once the sub-expression is evaluated to a value `V`.

```k
    syntax KItem ::= setEnv ( Map ) | "pushEnv" | "popEnv"
 // ------------------------------------------------------
    rule <k> setEnv(RHO) => . ... </k>
         <env> _ => RHO </env>

    rule <k> pushEnv => . ... </k>
         <env> ENV </env>
         <envs> .List => ListItem(ENV) ... </envs>

    rule <k> V:Val ~> popEnv => setEnv(ENV) ~> V ... </k>
         <envs> ListItem(ENV) => .List ... </envs>
```

### Getters

The following auxiliary operations extract the list of identifiers and of expressions in a binding, respectively.

```k
    /* Matching */
    syntax MatchResult ::= "matchFailure"
                         | matchResult    ( Bindings )
                         | matchResultAdd ( Bindings , Name , Val , Bindings )
 // --------------------------------------------------------------------------
    rule <k> matchFailure ~> (_:MatchResult => .) ... </k>

    rule <k> _:MatchResult ~> (matchResult(.Bindings) => .) ... </k>
    rule <k> (matchResult(BS) ~> matchResult(N' = V' and BS') => matchResultAdd(BS, N', V', .Bindings) ~> matchResult(BS')) ... </k>

    rule <k> matchResultAdd(.Bindings, N', V', BS') => matchResult(N' = V' and BS') ... </k>

    rule <k> matchResultAdd(N:Name = V:Val and BS, N', V', BS') => matchResultAdd(BS, N', V' , N = V and BS') ... </k> requires N =/=K N'  orBool V  ==K V'
    rule <k> matchResultAdd(N:Name = V:Val and BS, N', V', BS') => matchFailure                               ... </k> requires N  ==K N' andBool V =/=K V'

    syntax MatchResult ::= getMatching         ( Exp  , Val  )
                         | getMatchings        ( Exps , Vals )
                         | getMatchingBindings ( Exp  , Val , Bindings )
 // --------------------------------------------------------------------
    rule <k> getMatchingBindings(E, V, BS) => getMatching(E, V) ~> matchResult(BS) ... </k>

    rule <k> matchResult(BS) ~> getMatchings(ES, VS') => getMatchings(ES, VS') ~> matchResult(BS) ... </k>
    rule <k> matchResult(BS) ~> getMatching (E , V  ) => getMatching (E , V  ) ~> matchResult(BS) ... </k>

    rule <k> getMatching(V:Val, V':Val) => matchResult(.Bindings) ... </k> requires V ==K V'

    rule <k> getMatching(N:Name, V:Val) => matchResult(N = V) ... </k>

    rule <k> getMatching(E:Exp E':Exp , CV:ConstructorVal V':Val) => getMatching(E, CV) ~> getMatching(E', V') ... </k>

    rule <k> getMatching([ ES ], [ VS ]) => getMatchings(ES, VS) ... </k>

    rule <k> getMatching(_, _) => matchFailure ... </k> [owise]

    rule <k> getMatchings(VS:Vals, VS':Vals) => matchResult(.Bindings) ... </k> requires VS ==K VS'

    rule <k> getMatchings(X:Name,            VS:Vals          ) => matchResult(X = #listTailMatch(VS))       ... </k>
    rule <k> getMatchings((E:Exp : ES:Exps), (V:Val : VS:Vals)) => getMatching(E, V) ~> getMatchings(ES, VS) ... </k>

    rule <k> getMatchings(_, _) => matchFailure ... </k> [owise]

    syntax Val ::= #listTailMatch ( Vals )
 // --------------------------------------
endmodule
```
