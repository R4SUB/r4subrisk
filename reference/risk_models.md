# Pre-Built Risk Models

Ready-made
[`risk_model()`](https://r4sub.github.io/r4subrisk/reference/risk_model.md)
objects for the common frameworks. `risk_model_fmea()` is the default
three-factor FMEA product. `risk_model_probability_impact()` is a
two-factor probability times impact matrix. `risk_model_ich_q9()` is a
qualitative ICH Q9 style ranking that collapses probability and impact
into acceptable, tolerable, and unacceptable levels.

## Usage

``` r
risk_model_fmea(config = risk_config_default())

risk_model_probability_impact()

risk_model_ich_q9()
```

## Arguments

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  used by the FMEA model to pick up custom RPN bands.

## Value

A `risk_model` object.

## Examples

``` r
risks <- data.frame(
  risk_id     = c("R001", "R002", "R003"),
  probability = c(4, 3, 2),
  impact      = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
apply_risk_model(risks, risk_model_probability_impact())
#> # A tibble: 3 × 5
#>   risk_id raw_score risk_score risk_level model             
#>   <chr>       <dbl>      <dbl> <chr>      <chr>             
#> 1 R001           20         80 critical   probability_impact
#> 2 R002           12         48 high       probability_impact
#> 3 R003            6         24 medium     probability_impact
apply_risk_model(risks, risk_model_ich_q9())
#> # A tibble: 3 × 5
#>   risk_id raw_score risk_score risk_level   model 
#>   <chr>       <dbl>      <dbl> <chr>        <chr> 
#> 1 R001            9      100   unacceptable ich_q9
#> 2 R002            6       66.7 unacceptable ich_q9
#> 3 R003            2       22.2 acceptable   ich_q9
```
