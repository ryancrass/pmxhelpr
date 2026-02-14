# Determine axis breaks automatically for time variables

Determine axis breaks automatically for time variables

## Usage

``` r
breaks_time(x, unit = "hours", n = 8)
```

## Arguments

- x:

  Numeric vector of times from which to determine breaks

- unit:

  Character string for time units. Options include:

  - "hours" (default), "hrs", "hour", "hr", "h"

  - "days", "dys", "day", dy", "d"

  - "weeks", "wks", ,"week," "wk", "w"

  - "months", "mons", "mos", "month", "mo", "m"

- n:

  Ideal number of axis breaks requested (default = 8). Passed to
  [`labeling::extended()`](https://rdrr.io/pkg/labeling/man/extended.html)

## Value

A numeric vector of breaks

## Examples

``` r
ntimes <- sort(unique(data_sad$NTIME))
breaks <- breaks_time(ntimes)

```
