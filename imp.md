IMP Language
============

Implementations
---------------

-   <imp-k.md>: Implementation in K.

Description
-----------

IMP is a simple imperative language which supports:

-   Declaring new variables:

    ```imp
    int x, y, z, blah;
    ```

-   Assigning integers to variables:

    ```imp
    x = 3;
    ```

-   Arithmetic expressions:

    ```imp
    x = 3 * (y + z) / 2 - 7;
    ```

-   Conditionals with boolean expressions:

    ```imp
    if (! (x < (3 * y))) {
        z = 1;
    } else {
        z = -1;
    }
    ```

-   and Looping:

    ```imp
    while (x < 30) {
        y = y * x;
        x = x + 1;
    }
    ```

Examples
--------

### Sum to 10

```imp
int n, s;
n = 10;
s = 0;
while (0 < n) {
    s = s + n;
    n = n - 1;
}
```

### Find 1033rd prime

```imp
int n, nprimes, curprime, tester;
n        = 1033;
nprimes  = 1;
curprime = 2;
tester   = 1;

while (nprimes < n) {
    curprime = curprime + 1;
    tester = 2;
    while (tester < curprime && ! ((curprime / tester) * tester == curprime) {
        tester = tester + 1;
    }
    if (tester == curprime) {
        nprimes = nprimes + 1;
    }
}
```
