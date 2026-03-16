# Compare Risk Registers (Trend Analysis)

Compares two risk register snapshots and reports changes in RPN, new
risks, resolved risks, and risk level transitions.

## Usage

``` r
compare_risk_registers(before, after)
```

## Arguments

- before:

  A `risk_register` (earlier snapshot).

- after:

  A `risk_register` (later snapshot).

## Value

A list with:

- `rpn_changes`: tibble of risks with changed RPN

- `new_risks`: risk_ids present in `after` but not `before`

- `resolved_risks`: risk_ids present in `before` but not `after`

- `level_transitions`: tibble of risk level changes

- `delta_mean_rpn`: change in mean RPN

## Examples

``` r
r1 <- data.frame(
  risk_id = c("R001", "R002"),
  description = c("Missing vars", "Bad derivation"),
  probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
)
r2 <- data.frame(
  risk_id = c("R001", "R003"),
  description = c("Missing vars", "New issue"),
  probability = c(2, 3), impact = c(5, 3), detectability = c(2, 2)
)
rr1 <- create_risk_register(r1)
rr2 <- create_risk_register(r2)
compare_risk_registers(rr1, rr2)
#> $rpn_changes
#> # A tibble: 1 × 4
#>   risk_id rpn_before rpn_after rpn_delta
#>   <chr>        <dbl>     <dbl>     <dbl>
#> 1 R001            40        20       -20
#> 
#> $new_risks
#> [1] "R003"
#> 
#> $resolved_risks
#> [1] "R002"
#> 
#> $level_transitions
#> # A tibble: 1 × 4
#>   risk_id level_before level_after changed
#>   <chr>   <chr>        <chr>       <lgl>  
#> 1 R001    high         medium      TRUE   
#> 
#> $delta_mean_rpn
#> [1] -19
#> 
```
