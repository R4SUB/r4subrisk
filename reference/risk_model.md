# Define a Risk Scoring Model

Wraps a scoring rule so the engine can score risks with something other
than the default FMEA product. A model turns a data.frame of risks into
a raw score, a normalized 0–100 risk score, and a qualitative level. Use
the pre-built models or supply your own `score` function.

## Usage

``` r
risk_model(name, score, max_raw, bands, required = character())
```

## Arguments

- name:

  A short model name.

- score:

  A function taking a data.frame of risks and returning a numeric vector
  of raw scores, one per row.

- max_raw:

  The raw score at maximum risk, used to normalize to 0–100.

- bands:

  A named list of `c(lower, upper)` boundaries over the raw score,
  ordered from highest risk to lowest. Evaluated in order; first match
  wins.

- required:

  Character vector of columns the `score` function needs.

## Value

An object of class `"risk_model"`.

## Examples

``` r
risks <- data.frame(
  risk_id     = c("R001", "R002", "R003"),
  probability = c(4, 3, 2),
  impact      = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
m <- risk_model(
  name = "impact_only",
  score = function(df) df$impact,
  max_raw = 5,
  bands = list(high = c(4, 5), medium = c(3, 3), low = c(1, 2)),
  required = "impact"
)
apply_risk_model(risks, m)
#> # A tibble: 3 × 5
#>   risk_id raw_score risk_score risk_level model      
#>   <chr>       <dbl>      <dbl> <chr>      <chr>      
#> 1 R001            5        100 high       impact_only
#> 2 R002            4         80 high       impact_only
#> 3 R003            3         60 medium     impact_only
```
