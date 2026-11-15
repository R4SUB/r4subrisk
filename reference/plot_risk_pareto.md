# Plot a Risk Pareto Chart

Draws the classic Pareto chart for a risk register: a bar per risk,
ordered by descending RPN, with the running cumulative RPN share
overlaid as a line. The vital-few bars are highlighted and the cutoff is
drawn as a reference line. Reporting only; it recomputes no scores.

## Usage

``` r
plot_risk_pareto(risk_register, cum_cutoff = 0.8)
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
  or a tibble returned by
  [`risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.md).

- cum_cutoff:

  The cumulative RPN share that defines the vital few, forwarded to
  [`risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.md)
  when `risk_register` is a register. Defaults to `0.8`.

## Value

A `ggplot` object.

## See also

[`risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.md).

## Examples

``` r
if (FALSE) { # \dontrun{
rr <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002", "R003", "R004"),
  description   = c("a", "b", "c", "d"),
  probability   = c(4, 3, 2, 1),
  impact        = c(5, 4, 3, 2),
  detectability = c(2, 3, 4, 1)
))
plot_risk_pareto(rr)
} # }
```
