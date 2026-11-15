# Plot a Risk Burndown

Draws the milestone-over-milestone burndown produced by
[`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md)
as a line-and-point chart. Reporting only; it recomputes no scores.

## Usage

``` r
plot_risk_trend(
  registers,
  metric = c("total_rpn", "mean_rpn", "n_open_critical"),
  config = risk_config_default()
)
```

## Arguments

- registers:

  A named list of `risk_register` objects, as passed to
  [`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md),
  or the tibble returned by
  [`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md).

- metric:

  The series to plot, one of `"total_rpn"`, `"mean_rpn"`, or
  `"n_open_critical"`.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  forwarded to
  [`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md)
  when `registers` is a list.

## Value

A `ggplot` object.

## See also

[`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md).

## Examples

``` r
if (FALSE) { # \dontrun{
m1 <- create_risk_register(data.frame(
  risk_id = c("R001", "R002"), description = c("A", "B"),
  probability = c(4, 5), impact = c(5, 5), detectability = c(4, 4)
))
m2 <- apply_mitigations(m1, data.frame(risk_id = c("R001", "R002"),
                                        probability = c(2, 2)))
plot_risk_trend(list(baseline = m1, interim = m2))
} # }
```
