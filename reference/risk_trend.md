# Risk Burndown Across Submission Milestones

Summarizes a sequence of risk registers, one per submission milestone,
into a tidy burndown tibble. Where
[`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md)
and
[`risk_reduction_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_reduction_summary.md)
tell the before/after story of a single mitigation step, `risk_trend()`
tracks the whole trajectory, so a programme can show reviewers that risk
is falling milestone over milestone rather than at a single point in
time.

## Usage

``` r
risk_trend(registers, config = risk_config_default())
```

## Arguments

- registers:

  A named (or otherwise ordered) list of `risk_register` objects from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
  one per milestone, earliest first.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).
  Reserved for configurable classification; the registers' stored levels
  are used as-is.

## Value

A tibble of class `"risk_trend"` with one row per milestone and the
columns `milestone`, `milestone_index`, `n_risks`, `total_rpn`,
`mean_rpn`, and `n_open_critical`.

## Details

The registers are treated as an ordered sequence: the list order is the
milestone order (earliest first). Names, when supplied, are used as
milestone labels; an unnamed or blank entry falls back to
`milestone_<n>`. Each register keeps its own already-computed `rpn` and
`risk_level` (from
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
/
[`classify_rpn()`](https://r4sub.github.io/r4subrisk/reference/classify_rpn.md)),
so no scoring is recomputed here. The open-critical count is the number
of risks whose level is `"critical"` and whose `status` is `"open"`; an
empty register contributes a total and mean RPN of 0.

## See also

[`plot_risk_trend()`](https://r4sub.github.io/r4subrisk/reference/plot_risk_trend.md)
for the optional ggplot burndown,
[`risk_reduction_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_reduction_summary.md)
for a pairwise before/after summary.

## Examples

``` r
m1 <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002", "R003"),
  description   = c("Missing vars", "Bad derivation", "Define gap"),
  probability   = c(4, 5, 2),
  impact        = c(5, 5, 3),
  detectability = c(4, 4, 4)
))
m2 <- apply_mitigations(
  m1, data.frame(risk_id = c("R001", "R002"), probability = c(2, 2))
)
m3 <- apply_mitigations(
  m2, data.frame(risk_id = "R003", detectability = 1)
)
risk_trend(list(baseline = m1, interim = m2, final = m3))
#> # A tibble: 3 × 6
#>   milestone milestone_index n_risks total_rpn mean_rpn n_open_critical
#> * <chr>               <int>   <int>     <dbl>    <dbl>           <int>
#> 1 baseline                1       3       204     68                 2
#> 2 interim                 2       3       104     34.7               0
#> 3 final                   3       3        86     28.7               0
```
