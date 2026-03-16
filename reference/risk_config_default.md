# Default Risk Configuration

Returns configuration for risk assessment including FMEA scale
definitions, RPN thresholds, and risk level classification bands.

## Usage

``` r
risk_config_default(
  rpn_bands = list(critical = c(80, 125), high = c(40, 79), medium = c(15, 39), low =
    c(1, 14)),
  evidence_severity_to_probability = c(info = 1, low = 2, medium = 3, high = 4, critical
    = 5),
  evidence_severity_to_impact = c(info = 1, low = 2, medium = 3, high = 4, critical = 5),
  default_detectability = 3
)
```

## Arguments

- rpn_bands:

  Named list of RPN band boundaries `c(lower, upper)`. Evaluated in
  order; first match wins.

- evidence_severity_to_probability:

  Named numeric vector mapping evidence severity to probability scores
  (1–5 scale).

- evidence_severity_to_impact:

  Named numeric vector mapping evidence severity to impact scores (1–5
  scale).

- default_detectability:

  Default detectability score (1–5) when not explicitly provided. Lower
  = more detectable.

## Value

A list of class `"risk_config"` with elements: `rpn_bands`,
`evidence_severity_to_probability`, `evidence_severity_to_impact`,
`default_detectability`.

## Details

The FMEA-inspired risk model uses three dimensions:

- **Probability** (1–5): likelihood of the issue occurring/persisting

- **Impact** (1–5): severity of consequence if unresolved

- **Detectability** (1–5): difficulty of detecting the issue (1 = easy,
  5 = hard)

**RPN** = Probability x Impact x Detectability (range 1–125)

## Examples

``` r
cfg <- risk_config_default()
cfg$rpn_bands
#> $critical
#> [1]  80 125
#> 
#> $high
#> [1] 40 79
#> 
#> $medium
#> [1] 15 39
#> 
#> $low
#> [1]  1 14
#> 
```
