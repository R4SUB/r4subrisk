# Compute Risk Indicator Summary

Computes summary risk indicators from a risk register, similar to
`r4subtrace::trace_indicator_scores()`.

## Usage

``` r
risk_indicator_summary(risk_register)
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

## Value

A tibble with columns: `indicator`, `value`, `description`.

## Examples

``` r
risks <- data.frame(
  risk_id = c("R001", "R002", "R003"),
  description = c("Missing vars", "Bad derivation", "Label mismatch"),
  probability = c(4, 2, 1), impact = c(5, 3, 2),
  detectability = c(2, 3, 1)
)
rr <- create_risk_register(risks)
risk_indicator_summary(rr)
#> # A tibble: 7 × 3
#>   indicator           value description                               
#>   <chr>               <dbl> <chr>                                     
#> 1 RISK_TOTAL_COUNT     3    Total number of identified risks          
#> 2 RISK_OPEN_COUNT      3    Number of open (unresolved) risks         
#> 3 RISK_CRITICAL_COUNT  0    Number of critical-level risks            
#> 4 RISK_HIGH_COUNT      1    Number of high-level risks                
#> 5 RISK_MEAN_RPN       20    Mean Risk Priority Number across all risks
#> 6 RISK_MAX_RPN        40    Maximum Risk Priority Number              
#> 7 RISK_OVERALL_SCORE   0.16 Overall risk score (0=none, 1=maximum)    
```
