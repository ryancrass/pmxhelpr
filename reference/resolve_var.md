# Internal helper: Resolve function input conflicts to standard internal string constants

Internal helper: Resolve function input conflicts to standard internal
string constants

## Usage

``` r
resolve_var(quo, nullable = FALSE)
```

## Arguments

- quo:

  Function argument as a quosure (wrapped in rlang::ensym)

- nullable:

  Logical argument specifying if the argument may be `NULL`. Default is
  `FALSE`.

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
