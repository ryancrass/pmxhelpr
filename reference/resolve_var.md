# Internal helper: Resolve function input conflicts to standard internal string constants

Resolves a column argument to a string, in this order: a string literal
is used as is; a bare name found in `cols` is that column; otherwise the
expression is evaluated and a string result is used (so a variable
holding a column name works); failing that, a bare name is used as the
column name.

## Usage

``` r
resolve_var(quo, nullable = FALSE, cols = NULL, call = rlang::caller_env())
```

## Arguments

- quo:

  Function argument as a quosure (wrapped in rlang::ensym)

- nullable:

  Logical argument specifying if the argument may be `NULL`. Default is
  `FALSE`.

- cols:

  Names the argument is looked up in (e.g. `names(data)`, or a model's
  outputs), so a bare name matching one of them is never replaced by a
  same-named variable. `NULL` (default) skips that check.

- call:

  The call reported in the error.

## Value

A resolved string constant variable

## Examples

``` r
f <- function(x = DV) pmxhelpr:::resolve_var(rlang::enquo(x))
f()
#> [1] "DV"
f("ODV")
#> [1] "ODV"
f(CONC)
#> [1] "CONC"
```
