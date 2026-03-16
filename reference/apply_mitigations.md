# Update Risk Mitigation Status

Applies mitigation updates to a risk register. Allows updating
probability, impact, detectability, status, and mitigation notes for
specific risks.

## Usage

``` r
apply_mitigations(risk_register, updates, config = risk_config_default())
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- updates:

  A data.frame with at minimum column `risk_id`, plus any columns to
  update: `probability`, `impact`, `detectability`, `mitigation`,
  `status`.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

## Value

An updated `risk_register` with recomputed RPN and risk levels.

## Examples

``` r
risks <- data.frame(
  risk_id = c("R001", "R002"),
  description = c("Missing vars", "Bad derivation"),
  probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
)
rr <- create_risk_register(risks)

updates <- data.frame(
  risk_id     = "R001",
  probability = 2,
  mitigation  = "Added validation check",
  status      = "mitigated"
)
rr2 <- apply_mitigations(rr, updates)
rr2
#> ℹ Risk Register: 2 risk(s), 1 open
#> ℹ   Critical: 0, High: 0, Mean RPN: 28
#> # A tibble: 2 × 11
#>   risk_id description probability impact detectability category owner mitigation
#> * <chr>   <chr>             <dbl>  <dbl>         <dbl> <chr>    <chr> <chr>     
#> 1 R001    Missing va…           2      5             2 general  NA    Added val…
#> 2 R002    Bad deriva…           3      4             3 general  NA    NA        
#> # ℹ 3 more variables: status <chr>, rpn <dbl>, risk_level <chr>
```
