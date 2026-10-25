# Build Risk Heatmap Data

Aggregates a risk register into the probability by impact grid that an
FMEA heatmap is drawn from. Every cell of the 5 by 5 grid is returned,
including empty ones, so the plotted heatmap has no gaps.

## Usage

``` r
risk_heatmap_data(risk_register, config = risk_config_default())
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  used to assign a zone to each cell from the cell's mean RPN.

## Value

A tibble with one row per grid cell: `probability`, `impact`, `n` (risks
in the cell), `mean_rpn`, `max_rpn`, and `zone` (the risk level of the
cell's mean RPN, `NA` for empty cells).

## Examples

``` r
rr <- create_risk_register(data.frame(
  risk_id = c("R001", "R002"),
  description = c("a", "b"),
  probability = c(4, 2), impact = c(5, 3), detectability = c(2, 3)
))
risk_heatmap_data(rr)
#> # A tibble: 25 × 6
#>    probability impact     n mean_rpn max_rpn zone 
#>          <int>  <int> <int>    <dbl>   <dbl> <chr>
#>  1           1      1     0       NA      NA NA   
#>  2           2      1     0       NA      NA NA   
#>  3           3      1     0       NA      NA NA   
#>  4           4      1     0       NA      NA NA   
#>  5           5      1     0       NA      NA NA   
#>  6           1      2     0       NA      NA NA   
#>  7           2      2     0       NA      NA NA   
#>  8           3      2     0       NA      NA NA   
#>  9           4      2     0       NA      NA NA   
#> 10           5      2     0       NA      NA NA   
#> # ℹ 15 more rows
```
