# Score Risks with a Model

Applies a
[`risk_model()`](https://r4sub.github.io/r4subrisk/reference/risk_model.md)
to a set of risks and returns per-risk raw scores, normalized 0–100 risk
scores, and qualitative levels. Any model that produces a 0–100 risk
score plugs into the same Risk pillar computation.

## Usage

``` r
apply_risk_model(risks, model = risk_model_fmea())
```

## Arguments

- risks:

  A data.frame or `risk_register` with the columns the model requires.
  Must include `risk_id`.

- model:

  A `risk_model`. Defaults to
  [`risk_model_fmea()`](https://r4sub.github.io/r4subrisk/reference/risk_models.md).

## Value

A tibble with columns `risk_id`, `raw_score`, `risk_score` (0–100,
higher is more risk), `risk_level`, and `model`.

## Examples

``` r
risks <- data.frame(
  risk_id     = c("R001", "R002", "R003"),
  probability = c(4, 3, 2),
  impact      = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
apply_risk_model(risks, risk_model_fmea())
#> # A tibble: 3 × 5
#>   risk_id raw_score risk_score risk_level model
#>   <chr>       <dbl>      <dbl> <chr>      <chr>
#> 1 R001           40       32   high       fmea 
#> 2 R002           36       28.8 medium     fmea 
#> 3 R003           24       19.2 medium     fmea 
```
