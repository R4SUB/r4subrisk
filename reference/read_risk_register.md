# Read a Risk Register from Disk

Imports a risk register previously written by
[`write_risk_register()`](https://r4sub.github.io/r4subrisk/reference/write_risk_register.md)
(or any compatible CSV / xlsx carrying at least `risk_id` and
`description`) and rebuilds it through
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).
The canonical schema, RPN, and risk-level classification are therefore
reconstructed from the assessed scores rather than trusted from the
file, so an imported register is always internally consistent.

## Usage

``` r
read_risk_register(path, config = risk_config_default())
```

## Arguments

- path:

  Input file path. A `.xlsx` extension reads an Excel workbook (requires
  the readxl package); any other extension reads CSV.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  forwarded to
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
  for RPN classification.

## Value

A tibble of class `"risk_register"`.

## See also

[`write_risk_register()`](https://r4sub.github.io/r4subrisk/reference/write_risk_register.md)
for the inverse,
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

## Examples

``` r
rr <- create_risk_register(data.frame(
  risk_id       = c("R001", "R002"),
  description   = c("Missing vars", "Bad derivation"),
  probability   = c(4, 2),
  impact        = c(5, 3),
  detectability = c(2, 3)
))
path <- file.path(tempdir(), "risk_register.csv")
write_risk_register(rr, path)
rr2 <- read_risk_register(path)
identical(rr$rpn, rr2$rpn)
#> [1] TRUE
unlink(path)
```
