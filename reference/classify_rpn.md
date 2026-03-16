# Classify RPN Value into Risk Level

Classify RPN Value into Risk Level

## Usage

``` r
classify_rpn(rpn, bands = risk_config_default()$rpn_bands)
```

## Arguments

- rpn:

  Numeric RPN score (1–125).

- bands:

  Named list of band boundaries from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

## Value

Character risk level name.

## Examples

``` r
classify_rpn(90)
#> [1] "critical"
classify_rpn(25)
#> [1] "medium"
classify_rpn(5)
#> [1] "low"
```
