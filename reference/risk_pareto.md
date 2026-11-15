# Pareto Prioritization of a Risk Register

Orders the risks in a register from highest to lowest Risk Priority
Number and reports, for each risk, its share of the total RPN and the
running cumulative share. This is the classic Pareto ("vital few") view:
it surfaces the small set of risks that together drive most of the
register's RPN, so a programme can direct mitigation effort where it
removes the most risk first.

## Usage

``` r
risk_pareto(risk_register, cum_cutoff = 0.8)
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- cum_cutoff:

  The cumulative RPN share that defines the vital few, a single number
  in `(0, 1]`. Defaults to `0.8`.

## Value

A tibble of class `"risk_pareto"`, ordered by descending RPN, with
columns `rank`, `risk_id`, `description`, `category`, `rpn`,
`risk_level`, `rpn_share`, `cum_rpn`, `cum_share`, and `vital_few`. The
cutoff used, the register total RPN, and the number of vital-few risks
are attached as the attributes `cum_cutoff`, `total_rpn`, and
`n_vital_few`.

## Details

No scoring is recomputed. The already-stored `rpn` from
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
is used as-is, and risks are ranked by it (ties are broken by `risk_id`
so the order is deterministic). `rpn_share` is each risk's RPN divided
by the register total; `cum_share` is the running sum of those shares
down the ranking.

A risk is flagged in `vital_few` when it falls inside the shortest run
of top-ranked risks whose cumulative share first reaches `cum_cutoff`.
The cutoff is a reporting convention supplied by the caller (its
default, `0.8`, is the usual Pareto 80 percent) and is applied to the
cumulative RPN share only; it is not a risk, severity, or RPN threshold
and does not change any risk level. When the total RPN is zero (for
example an empty register) no risk is flagged.

## See also

[`plot_risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/plot_risk_pareto.md)
for the optional ggplot Pareto chart,
[`compute_risk_scores()`](https://r4sub.github.io/r4subrisk/reference/compute_risk_scores.md)
for register-level aggregates.

## Examples

``` r
rr <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002", "R003", "R004"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                    "Inconsistent define.xml", "Label mismatches"),
  probability   = c(4, 3, 2, 1),
  impact        = c(5, 4, 3, 2),
  detectability = c(2, 3, 4, 1)
))
risk_pareto(rr)
#> ℹ Risk Pareto: 4 risk(s), total RPN 102
#> ℹ Vital few: 3 risk(s) (75% of risks) carry >= 80% of total RPN
#> # A tibble: 4 × 10
#>    rank risk_id description          category   rpn risk_level rpn_share cum_rpn
#> * <int> <chr>   <chr>                <chr>    <dbl> <chr>          <dbl>   <dbl>
#> 1     1 R001    Missing SDTM variab… general     40 high          0.392       40
#> 2     2 R002    Unmapped ADaM deriv… general     36 medium        0.353       76
#> 3     3 R003    Inconsistent define… general     24 medium        0.235      100
#> 4     4 R004    Label mismatches     general      2 low           0.0196     102
#> # ℹ 2 more variables: cum_share <dbl>, vital_few <lgl>
```
