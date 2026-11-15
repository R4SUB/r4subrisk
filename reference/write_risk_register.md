# Write a Risk Register to Disk

Exports a risk register to a flat file so it can be shared, reviewed
outside R, or re-imported later with
[`read_risk_register()`](https://r4sub.github.io/r4subrisk/reference/read_risk_register.md).
All columns are written, including the computed `rpn` and `risk_level`,
so the file is a faithful, human-readable snapshot. Those computed
columns are *not* trusted on re-import:
[`read_risk_register()`](https://r4sub.github.io/r4subrisk/reference/read_risk_register.md)
rebuilds the register through
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
recomputing the FMEA scores from the assessed probability, impact, and
detectability. A write -\> read cycle therefore reproduces an equivalent
register.

## Usage

``` r
write_risk_register(register, path)
```

## Arguments

- register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- path:

  Output file path. A `.xlsx` extension writes an Excel workbook
  (requires the writexl package); any other extension writes CSV.

## Value

`path`, invisibly.

## See also

[`read_risk_register()`](https://r4sub.github.io/r4subrisk/reference/read_risk_register.md)
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
read_risk_register(path)
#> ℹ Risk Register: 2 risk(s), 2 open
#> ℹ   Critical: 0, High: 1, Mean RPN: 29
#> # A tibble: 2 × 11
#>   risk_id description probability impact detectability category owner mitigation
#> * <chr>   <chr>             <dbl>  <dbl>         <dbl> <chr>    <chr> <chr>     
#> 1 R001    Missing va…           4      5             2 general  NA    NA        
#> 2 R002    Bad deriva…           2      3             3 general  NA    NA        
#> # ℹ 3 more variables: status <chr>, rpn <dbl>, risk_level <chr>
unlink(path)
```
