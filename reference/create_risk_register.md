# Create a Risk Register

Builds a risk register from a user-supplied data.frame of identified
risks. Validates required columns and fills defaults.

## Usage

``` r
create_risk_register(risks, config = risk_config_default())
```

## Arguments

- risks:

  A data.frame with at minimum columns `risk_id` and `description`.
  Optional columns: `category`, `probability`, `impact`,
  `detectability`, `owner`, `mitigation`, `status`.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

## Value

A tibble of class `"risk_register"` with standardized columns and
computed RPN values.

## Examples

``` r
risks <- data.frame(
  risk_id     = c("R001", "R002", "R003"),
  description = c("Missing SDTM variables", "Unmapped ADaM derivations",
                   "Inconsistent define.xml"),
  category    = c("data_quality", "traceability", "documentation"),
  probability = c(4, 3, 2),
  impact      = c(5, 4, 3),
  detectability = c(2, 3, 4)
)
rr <- create_risk_register(risks)
rr
#> ℹ Risk Register: 3 risk(s), 3 open
#> ℹ   Critical: 0, High: 1, Mean RPN: 33.3
#> # A tibble: 3 × 11
#>   risk_id description category probability impact detectability owner mitigation
#> * <chr>   <chr>       <chr>          <dbl>  <dbl>         <dbl> <chr> <chr>     
#> 1 R001    Missing SD… data_qu…           4      5             2 NA    NA        
#> 2 R002    Unmapped A… traceab…           3      4             3 NA    NA        
#> 3 R003    Inconsiste… documen…           2      3             4 NA    NA        
#> # ℹ 3 more variables: status <chr>, rpn <dbl>, risk_level <chr>
```
