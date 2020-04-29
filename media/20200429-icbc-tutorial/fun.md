---
title: 'FUN: K Tutorial'
author:
-   Everett Hildenbrandt
-   Daejun Park
-   Yi Zhang
-   Grigore Roșu
institute:
-   Runtime Verification, Inc
date: '\today'
theme: metropolis
fontsize: 8pt
---

FUN Language
============

Repository Setup
----------------

Defined in the [k-tutorial](https://github.com/ehildenb/k-tutorial) repository.

### Dependencies

-   All of [K Framework](https://github.com/kframework/k) dependencies.
-   [pandoc](https://pandoc.org)
-   [TeX Live](https://tug.org/texlive)

### Build

```sh
git clone 'https://github.com/ehildenb/k-tutorial'
cd k-tutorial
make deps
make build -j8
```

### Test

```sh
make test -j8
```

Running Programs
----------------

Use the `./fun` runner script.

For help:

```sh
./fun help
```

And running a sample program (with optional depth bound):

```sh
./fun run tests/fun/sum.fun
./fun run tests/fun/sum.fun --depth 40
```

Stepping Through an Execution
-----------------------------

Program `sum-tail-recursive.fun`:

```fun
letrec
    sumrec = fun 0 m -> m
               | n m -> sumrec (n - 1) (m + n)
and sum n = sumrec n 0
 in sum 10
```

Run with various depth bounds:

```sh
./fun run tests/fun/sum-tail-recursive.fun --depth 0
./fun run tests/fun/sum-tail-recursive.fun --depth 17
./fun run tests/fun/sum-tail-recursive.fun --depth 24
./fun run tests/fun/sum-tail-recursive.fun --depth 25
./fun run tests/fun/sum-tail-recursive.fun --depth 35
./fun run tests/fun/sum-tail-recursive.fun --depth 38
./fun run tests/fun/sum-tail-recursive.fun --depth 45
./fun run tests/fun/sum-tail-recursive.fun --depth 46
./fun run tests/fun/sum-tail-recursive.fun --depth 49
./fun run tests/fun/sum-tail-recursive.fun --depth 50
./fun run tests/fun/sum-tail-recursive.fun --depth 51
./fun run tests/fun/sum-tail-recursive.fun --depth 55
./fun run tests/fun/sum-tail-recursive.fun --depth 56
./fun run tests/fun/sum-tail-recursive.fun --depth 60
./fun run tests/fun/sum-tail-recursive.fun --depth 63
./fun run tests/fun/sum-tail-recursive.fun --depth 66
./fun run tests/fun/sum-tail-recursive.fun --depth 134
./fun run tests/fun/sum-tail-recursive.fun --depth 582
./fun run tests/fun/sum-tail-recursive.fun --depth 583
./fun run tests/fun/sum-tail-recursive.fun --depth 586
./fun run tests/fun/sum-tail-recursive.fun --depth 590
./fun run tests/fun/sum-tail-recursive.fun --depth 595
./fun run tests/fun/sum-tail-recursive.fun --depth 598
./fun run tests/fun/sum-tail-recursive.fun --depth 605
./fun run tests/fun/sum-tail-recursive.fun --depth 609
./fun run tests/fun/sum-tail-recursive.fun
```
