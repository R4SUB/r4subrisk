# Compute Risk Scores from a Risk Register

Computes aggregate risk metrics from a risk register, including mean
RPN, risk distribution, and overall risk score normalized to 0–1.

## Usage

``` r
compute_risk_scores(risk_register, config = risk_config_default())
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

## Value

A list of class `"risk_scores"` with:

- `overall_risk_score`: 0–1 (0 = no risk, 1 = maximum risk)

- `mean_rpn`: average RPN across all risks

- `max_rpn`: highest individual RPN

- `n_risks`: total risk count

- `risk_distribution`: tibble of counts by risk_level

- `category_summary`: tibble of mean RPN by category

## Examples

``` r
risks <- data.frame(
  risk_id = c("R001", "R002"),
  description = c("Missing vars", "Bad derivation"),
  probability = c(4, 2), impact = c(5, 3), detectability = c(2, 3)
)
rr <- create_risk_register(risks)
compute_risk_scores(rr)
#> ℹ Risk Score Summary
#> ℹ   Overall risk score: 0.232 (0=none, 1=max)
#> ℹ   Mean RPN: 29, Max RPN: 40
#> ℹ   Total risks: 2
#> ℹ     high: 1
#> ℹ     medium: 1
```
