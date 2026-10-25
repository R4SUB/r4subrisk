# Plot an FMEA Risk Heatmap

Draws the standard FMEA probability by impact heatmap. Cells are
coloured by the chosen metric and labelled with the number of risks that
fall in them.

## Usage

``` r
plot_risk_heatmap(
  risk_register,
  metric = c("mean_rpn", "max_rpn", "n"),
  config = risk_config_default(),
  interactive = FALSE
)
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- metric:

  The cell fill, one of `"mean_rpn"`, `"max_rpn"`, or `"n"`.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

- interactive:

  If `TRUE`, return a `plotly` widget instead of a `ggplot`. Requires
  the `plotly` package.

## Value

A `ggplot` object, or a `plotly` object when `interactive = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
risks <- data.frame(
  risk_id       = c("R001", "R002", "R003"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                    "Inconsistent define.xml"),
  category      = c("data_quality", "traceability", "documentation"),
  probability   = c(4, 3, 2),
  impact        = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
rr <- create_risk_register(risks)
plot_risk_heatmap(rr)
} # }
```
