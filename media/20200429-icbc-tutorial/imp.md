---
title: 'IMP: K Tutorial'
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

IMP Language
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
make deps
make build -j8
```

### Test

```sh
make test -j8
```

Running Programs
----------------

Use the `./imp` runner script.

For help:

```sh
./imp help
```

And running a sample program (with optional depth bound):

```sh
./imp run tests/imp/sum.imp
./imp run tests/imp/sum.imp --depth 40
```

Proving Properties
------------------

Use the `./imp` runner script.

For help:

```sh
./imp help
```

And proving a sample proprety:

```sh
./imp prove tests/imp/sum-proc-spec.k VERIFICATION
```

Stepping Through an Execution
-----------------------------

Program `sum.imp`:

```imp
int n, s;
n = 10;
s = 0;
while (! (n <= 0) ) {
    s = s + n;
    n = n - 1;
}
```

Run with various depth bounds:

```sh
./imp run tests/imp/sum.imp --depth 0
./imp run tests/imp/sum.imp --depth 8
./imp run tests/imp/sum.imp --depth 9
./imp run tests/imp/sum.imp --depth 10
./imp run tests/imp/sum.imp --depth 18
./imp run tests/imp/sum.imp --depth 19
./imp run tests/imp/sum.imp --depth 40
./imp run tests/imp/sum.imp
```
