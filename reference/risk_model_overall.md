# Overall Normalized Risk from a Scored Set

Reduces the per-risk output of
[`apply_risk_model()`](https://r4sub.github.io/r4subrisk/reference/apply_risk_model.md)
to a single normalized 0–100 risk figure (the mean risk score). This is
the value the Risk pillar consumes, and it is model-agnostic by
construction.

## Usage

``` r
risk_model_overall(scored)
```

## Arguments

- scored:

  A tibble from
  [`apply_risk_model()`](https://r4sub.github.io/r4subrisk/reference/apply_risk_model.md).

## Value

A single number in `[0, 100]`.

## Examples

``` r
risks <- data.frame(
  risk_id     = c("R001", "R002", "R003"),
  probability = c(4, 3, 2),
  impact      = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
scored <- apply_risk_model(risks, risk_model_ich_q9())
risk_model_overall(scored)
#> [1] 63
```
