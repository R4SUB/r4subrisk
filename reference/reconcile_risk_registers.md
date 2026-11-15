# Reconcile Two Risk Registers

Aligns two versions of a risk register by their risk identifier and
reports, item by item, exactly what changed between them: which risks
were added, removed, left unchanged, or changed, and for the changed
risks, which fields differ. Where
[`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md)
tracks the whole trajectory over many milestones and
[`risk_reduction_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_reduction_summary.md)
quantifies how much RPN was removed, `reconcile_risk_registers()` gives
the line-by-line audit of a single pair, as a reviewer comparing a prior
submission draft against the current one, or two assessors' independent
registers, would want to see.

## Usage

``` r
reconcile_risk_registers(
  prior,
  current,
  fields = c("description", "category", "probability", "impact", "detectability", "owner",
    "mitigation", "status"),
  config = risk_config_default()
)
```

## Arguments

- prior:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md):
  the earlier or baseline version.

- current:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md):
  the later or comparison version.

- fields:

  Character vector of column names whose differences define a
  `"changed"` risk. Defaults to the assessment and management fields
  `c("description", "category", "probability", "impact", "detectability", "owner", "mitigation", "status")`.
  Every name must be a column of both registers.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).
  Accepted for interface consistency with the other register functions;
  the stored RPN and levels are used as-is, so it currently influences
  nothing.

## Value

A list of class `"risk_reconciliation"` with:

- `per_risk`: a tibble with one row per risk in the union of the two
  registers and columns `risk_id`, `change_type`, `n_fields_changed`,
  `changed_fields` (comma-separated, `NA` when none), `rpn_prior`,
  `rpn_current`, `rpn_delta`, `level_prior`, and `level_current`;

- `field_changes`: a long tibble of every differing field on the changed
  risks, with columns `risk_id`, `field`, `value_prior`, and
  `value_current` (values rendered as character for a uniform audit
  column);

- scalar counts `n_prior`, `n_current`, `n_added`, `n_removed`,
  `n_changed`, `n_unchanged`;

- `total_rpn_prior`, `total_rpn_current`, `total_rpn_delta`, the
  register-level RPN movement over the stored values;

- `fields`, the tracked field names used.

## Details

The two registers are matched on `risk_id` (unique within a register,
since
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
rejects duplicates). Each risk in the union of the two is assigned one
`change_type`:

- `"added"` — present in `current` but not in `prior`;

- `"removed"` — present in `prior` but not in `current`;

- `"changed"` — present in both, with at least one tracked field
  differing;

- `"unchanged"` — present in both, with every tracked field equal.

Two values are treated as equal when both are `NA`, and as differing
when one is `NA` and the other is not. No scores, scales, bands, or
thresholds are invented or recomputed: this is a purely structural diff.
RPN movement is read from each register's own stored `rpn` (the value
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
already computed), and `risk_level` is likewise read as stored; both are
reported as movement rather than as tracked change fields, because they
are derived from `probability`, `impact`, and `detectability` rather
than edited directly.

## See also

[`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md)
for a lighter RPN/level diff,
[`risk_reduction_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_reduction_summary.md)
for the before/after reduction magnitude,
[`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md)
for the trajectory across many milestones.

## Examples

``` r
prior <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002", "R003"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                    "Inconsistent define.xml"),
  probability   = c(4, 3, 2),
  impact        = c(5, 4, 3),
  detectability = c(2, 3, 4),
  status        = c("open", "open", "open")
))
current <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002", "R004"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                    "Late-breaking lab data"),
  probability   = c(2, 3, 3),
  impact        = c(5, 4, 4),
  detectability = c(2, 3, 2),
  status        = c("mitigated", "open", "open")
))
rec <- reconcile_risk_registers(prior, current)
rec
#> ℹ Risk Reconciliation: 3 prior risk(s) -> 3 current
#> ℹ Added: 1, removed: 1, changed: 1, unchanged: 1
#> ℹ Total RPN: 100 -> 80 (-20 delta)
rec$per_risk
#> # A tibble: 4 × 9
#>   risk_id change_type n_fields_changed changed_fields      rpn_prior rpn_current
#>   <chr>   <chr>                  <int> <chr>                   <dbl>       <dbl>
#> 1 R001    changed                    2 probability, status        40          20
#> 2 R004    added                      0 NA                         NA          24
#> 3 R003    removed                    0 NA                         24          NA
#> 4 R002    unchanged                  0 NA                         36          36
#> # ℹ 3 more variables: rpn_delta <dbl>, level_prior <chr>, level_current <chr>
rec$field_changes
#> # A tibble: 2 × 4
#>   risk_id field       value_prior value_current
#>   <chr>   <chr>       <chr>       <chr>        
#> 1 R001    probability 4           2            
#> 2 R001    status      open        mitigated    
```
