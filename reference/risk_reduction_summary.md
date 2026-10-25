# Summarize Risk Reduction Between Two Registers

Quantifies how much risk was removed between an earlier and a later risk
register, so a mitigation effort can be reported as a single
before/after story. Regulatory reviewers expect to see that identified
risks were acted on, not just listed.

## Usage

``` r
risk_reduction_summary(before, after, config = risk_config_default())
```

## Arguments

- before:

  A `risk_register` (earlier snapshot).

- after:

  A `risk_register` (later snapshot, usually the result of
  [`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md)).

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  used to order the risk-level bands when deciding what counts as a
  downgrade.

## Value

A list of class `"risk_reduction"` with overall figures and a `per_risk`
tibble of the common risks. Fields include `total_rpn_before`,
`total_rpn_after`, `total_rpn_reduction`, `pct_reduction`,
`mean_rpn_before`, `mean_rpn_after`, `n_reduced`, `n_increased`,
`n_unchanged`, `n_downgraded`, `n_added`, `n_resolved`, and `per_risk`.

## Details

Reduction is measured on the risks common to both registers. Total RPN
is the sum of Risk Priority Numbers, mean RPN is their average, and the
percentage reduction is relative to the before total. A risk is counted
as `downgraded` when its risk level moves to a lower band (for example
`critical` to `high`). Risks that appear only in `after` are reported as
`n_added`, and risks that disappear are reported as `n_resolved`.

## Examples

``` r
risks <- data.frame(
  risk_id = c("R001", "R002"),
  description = c("Missing vars", "Bad derivation"),
  probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
)
rr <- create_risk_register(risks)
rr2 <- apply_mitigations(rr, data.frame(risk_id = "R001", probability = 1))
risk_reduction_summary(rr, rr2)
#> ℹ Risk Reduction Summary
#> ℹ Total RPN: 76 -> 46 (30 down, 39.5%)
#> ℹ   Mean RPN: 38 -> 23
#> ℹ   Risks reduced: 1, downgraded a level: 1
```
