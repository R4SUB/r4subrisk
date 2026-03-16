# Risk Management with r4subrisk

Clinical submissions require a systematic approach to identifying,
scoring, and mitigating risks. The `r4subrisk` package implements an
FMEA (Failure Mode and Effects Analysis) based risk framework aligned
with ICH Q9 quality risk management principles.

``` r
library(r4subrisk)
```

## Default configuration

[`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md)
defines the RPN (Risk Priority Number) band thresholds and scoring
scales:

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
cfg$required_origins
#> NULL
```

## Creating a risk register

A risk register is built from a data frame with at minimum `risk_id`,
`description`, `probability`, `impact`, and `detectability` — each
scored 1–5. The RPN is computed as
`probability * impact * detectability`.

``` r
risks <- data.frame(
  risk_id       = paste0("RISK-00", 1:5),
  description   = c(
    "Missing required variables in ADSL",
    "Incomplete derivation documentation for ADTTE",
    "Independent QC not completed for ADRS",
    "Define-XML missing codelist references",
    "ADRG not included in submission package"
  ),
  category      = c("data_quality", "traceability", "process",
                    "documentation", "regulatory"),
  probability   = c(3L, 4L, 2L, 2L, 1L),
  impact        = c(4L, 5L, 5L, 3L, 5L),
  detectability = c(2L, 3L, 4L, 2L, 1L),
  owner         = c("Data Mgmt", "Stats", "Stats", "Programming", "Regulatory"),
  stringsAsFactors = FALSE
)

rr <- create_risk_register(risks)
print(rr)
#> ℹ Risk Register: 5 risk(s), 5 open
#> ℹ   Critical: 0, High: 2, Mean RPN: 28.2
#> # A tibble: 5 × 11
#>   risk_id description category probability impact detectability owner mitigation
#> * <chr>   <chr>       <chr>          <dbl>  <dbl>         <dbl> <chr> <chr>     
#> 1 RISK-0… Missing re… data_qu…           3      4             2 Data… NA        
#> 2 RISK-0… Incomplete… traceab…           4      5             3 Stats NA        
#> 3 RISK-0… Independen… process            2      5             4 Stats NA        
#> 4 RISK-0… Define-XML… documen…           2      3             2 Prog… NA        
#> 5 RISK-0… ADRG not i… regulat…           1      5             1 Regu… NA        
#> # ℹ 3 more variables: status <chr>, rpn <dbl>, risk_level <chr>
```

## Computing risk scores

[`compute_risk_scores()`](https://r4sub.github.io/r4subrisk/reference/compute_risk_scores.md)
returns aggregate metrics including overall risk score (normalized 0-1),
mean RPN, and per-category breakdown:

``` r
rs <- compute_risk_scores(rr)
rs$overall_risk_score
#> [1] 0.2256
rs$mean_rpn
#> [1] 28.2
rs$risk_distribution
#> # A tibble: 3 × 2
#>   risk_level     n
#>   <chr>      <int>
#> 1 high           2
#> 2 low            2
#> 3 medium         1
rs$category_summary
#> # A tibble: 5 × 3
#>   category          n mean_rpn
#>   <chr>         <int>    <dbl>
#> 1 data_quality      1       24
#> 2 documentation     1       12
#> 3 process           1       40
#> 4 regulatory        1        5
#> 5 traceability      1       60
```

## Applying mitigations

[`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md)
updates the register with post-control scores to show residual risk
after mitigations are applied:

``` r
controls <- data.frame(
  risk_id               = c("RISK-001", "RISK-002", "RISK-003"),
  mitigation            = c(
    "Automated variable check implemented in CI pipeline",
    "Derivation review added to QC checklist",
    "QC sign-off required before dataset lock"
  ),
  probability_residual  = c(1L, 2L, 1L),
  impact_residual       = c(4L, 5L, 5L),
  detectability_residual = c(1L, 1L, 1L),
  stringsAsFactors = FALSE
)

rr_mitigated <- apply_mitigations(rr, controls)
```

## Comparing registers

[`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md)
shows the RPN change before and after mitigations:

``` r
comparison <- compare_risk_registers(rr, rr_mitigated)
comparison
#> $rpn_changes
#> # A tibble: 5 × 4
#>   risk_id  rpn_before rpn_after rpn_delta
#>   <chr>         <dbl>     <dbl>     <dbl>
#> 1 RISK-001         24        24         0
#> 2 RISK-002         60        60         0
#> 3 RISK-003         40        40         0
#> 4 RISK-004         12        12         0
#> 5 RISK-005          5         5         0
#> 
#> $new_risks
#> character(0)
#> 
#> $resolved_risks
#> character(0)
#> 
#> $level_transitions
#> # A tibble: 5 × 4
#>   risk_id  level_before level_after changed
#>   <chr>    <chr>        <chr>       <lgl>  
#> 1 RISK-001 medium       medium      FALSE  
#> 2 RISK-002 high         high        FALSE  
#> 3 RISK-003 high         high        FALSE  
#> 4 RISK-004 low          low         FALSE  
#> 5 RISK-005 low          low         FALSE  
#> 
#> $delta_mean_rpn
#> [1] 0
```

## Integration with r4subcore evidence

[`evidence_to_risks()`](https://r4sub.github.io/r4subrisk/reference/evidence_to_risks.md)
converts an evidence table with `indicator_domain == "risk"` into a risk
register format, allowing automation of risk identification from
evidence outputs:

``` r
# Requires r4subcore
ctx <- r4subcore::r4sub_run_context(study_id = "STUDY01")
ev <- r4subcore::as_evidence(
  data.frame(
    asset_type = "program", asset_id = "prod_adrs.R",
    source_name = "r4subrisk", source_version = "0.1.0",
    indicator_id = "R-001", indicator_name = "Program Validation",
    indicator_domain = "risk", severity = "high",
    result = "fail", metric_value = 0, metric_unit = "score",
    message = "DVP not submitted", location = "prod_adrs.R",
    evidence_payload = "{}", stringsAsFactors = FALSE
  ), ctx = ctx
)
risk_df <- evidence_to_risks(ev)
rr_from_evidence <- create_risk_register(risk_df)
```
