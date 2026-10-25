# Case study: FMEA risk on a pilot submission

A risk log that only lists risks is a to-do list. What a reviewer wants
to see is that the risks were ranked, worked, and reduced. `r4subrisk`
puts numbers on that story using a Failure Modes and Effects Analysis
(FMEA) model: every risk gets a probability, an impact, and a
detectability score, and their product is the Risk Priority Number
(RPN).

This case study runs the example submission risk register from
`r4subdata` through that model, then shows what happens when the team
starts mitigating.

``` r

library(r4subrisk)
library(r4subdata)
```

## The register

`risk_register_pharma` is a worked submission risk log: 18 risks across
data quality, traceability, and documentation, each already scored on
the three FMEA dimensions.

``` r

risk_register_pharma[, c("risk_id", "category", "probability", "impact",
                         "detectability")]
#> # A tibble: 18 × 5
#>    risk_id  category      probability impact detectability
#>    <chr>    <chr>               <int>  <int>         <int>
#>  1 RISK-001 data_quality            3      4             2
#>  2 RISK-002 traceability            4      3             3
#>  3 RISK-003 documentation           2      4             2
#>  4 RISK-004 data_quality            3      3             3
#>  5 RISK-005 data_quality            2      3             1
#>  6 RISK-006 data_quality            4      4             3
#>  7 RISK-007 compliance              3      4             1
#>  8 RISK-008 traceability            4      3             4
#>  9 RISK-009 data_quality            2      5             3
#> 10 RISK-010 documentation           3      2             2
#> 11 RISK-011 compliance              3      4             2
#> 12 RISK-012 programming             2      3             3
#> 13 RISK-013 compliance              3      4             4
#> 14 RISK-014 data_quality            2      4             2
#> 15 RISK-015 documentation           3      3             2
#> 16 RISK-016 programming             2      5             3
#> 17 RISK-017 compliance              1      3             2
#> 18 RISK-018 documentation           3      3             3
```

[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)
validates the scales and computes the RPN and risk level for each row.

``` r

rr <- create_risk_register(risk_register_pharma)
rr
#> ℹ Risk Register: 18 risk(s), 12 open
#> ℹ   Critical: 0, High: 3, Mean RPN: 24.8
#> # A tibble: 18 × 11
#>    risk_id  description          category probability impact detectability owner
#>  * <chr>    <chr>                <chr>          <dbl>  <dbl>         <dbl> <chr>
#>  1 RISK-001 Missing required SD… data_qu…           3      4             2 Data…
#>  2 RISK-002 Unmapped ADaM deriv… traceab…           4      3             3 Stat…
#>  3 RISK-003 Define.xml incomple… documen…           2      4             2 Medi…
#>  4 RISK-004 Inconsistent date f… data_qu…           3      3             3 Data…
#>  5 RISK-005 Duplicate records f… data_qu…           2      3             1 Stat…
#>  6 RISK-006 Laboratory units no… data_qu…           4      4             3 Data…
#>  7 RISK-007 Pinnacle21 validati… complia…           3      4             1 Qual…
#>  8 RISK-008 Traceability gap: A… traceab…           4      3             4 Stat…
#>  9 RISK-009 Critical MedDRA cod… data_qu…           2      5             3 Medi…
#> 10 RISK-010 Reviewer note: exce… documen…           3      2             2 Stat…
#> 11 RISK-011 Missing controlled … complia…           3      4             2 Data…
#> 12 RISK-012 Unresolved programm… program…           2      3             3 Stat…
#> 13 RISK-013 Incomplete audit tr… complia…           3      4             4 Qual…
#> 14 RISK-014 Cross-domain refere… data_qu…           2      4             2 Data…
#> 15 RISK-015 Define.xml version … documen…           3      3             2 Medi…
#> 16 RISK-016 Statistical analysi… program…           2      5             3 Bios…
#> 17 RISK-017 eCTD folder structu… complia…           1      3             2 Regu…
#> 18 RISK-018 Insufficient docume… documen…           3      3             3 Bios…
#> # ℹ 4 more variables: mitigation <chr>, status <chr>, rpn <dbl>,
#> #   risk_level <chr>
```

## The risk picture

[`compute_risk_scores()`](https://r4sub.github.io/r4subrisk/reference/compute_risk_scores.md)
rolls the register up: the mean and maximum RPN, an overall 0 to 1 risk
score, and the spread of risk levels.

``` r

scores <- compute_risk_scores(rr)
scores
#> ℹ Risk Score Summary
#> ℹ   Overall risk score: 0.1982 (0=none, 1=max)
#> ℹ   Mean RPN: 24.8, Max RPN: 48
#> ℹ   Total risks: 18
#> ℹ     high: 3
#> ℹ     low: 4
#> ℹ     medium: 11
scores$category_summary
#> # A tibble: 5 × 3
#>   category          n mean_rpn
#>   <chr>         <int>    <dbl>
#> 1 compliance        4     22.5
#> 2 data_quality      6     25.2
#> 3 documentation     4     18.2
#> 4 programming       2     24  
#> 5 traceability      2     42
```

The category summary is where prioritisation starts. The categories
carrying the highest mean RPN are the ones to work first.

## Working the highest risks

Mitigation is the point of the exercise. Suppose the team addresses the
two highest-RPN risks: they add the missing SDTM variables and document
the ADaM derivation, which lowers both the probability and the
detectability difficulty.

``` r

ranked <- rr[order(-rr$rpn), c("risk_id", "description", "rpn", "risk_level")]
head(ranked, 3)
#> Warning: Unknown or uninitialised column: `status`.
#> ℹ Risk Register: 3 risk(s), 0 open
#> ℹ   Critical: 0, High: 3, Mean RPN: 48
#> # A tibble: 3 × 4
#>   risk_id  description                                           rpn risk_level
#>   <chr>    <chr>                                               <dbl> <chr>     
#> 1 RISK-006 Laboratory units not standardized across sites         48 high      
#> 2 RISK-008 Traceability gap: ADLB.BASE derivation undocumented    48 high      
#> 3 RISK-013 Incomplete audit trail for manual data corrections     48 high
```

[`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md)
takes the updates and recomputes the RPNs and levels.

``` r

top_two <- head(ranked$risk_id, 2)

updates <- data.frame(
  risk_id       = top_two,
  probability   = c(1, 2),
  detectability = c(1, 1),
  status        = "mitigated",
  stringsAsFactors = FALSE
)

rr_after <- apply_mitigations(rr, updates)
```

## Before and after

[`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md)
reports what moved: which RPNs dropped, which risk levels changed, and
the shift in the mean.

``` r

comparison <- compare_risk_registers(rr, rr_after)
comparison$rpn_changes[comparison$rpn_changes$rpn_delta != 0, ]
#> # A tibble: 2 × 4
#>   risk_id  rpn_before rpn_after rpn_delta
#>   <chr>         <dbl>     <dbl>     <dbl>
#> 1 RISK-006         48         4       -44
#> 2 RISK-008         48         6       -42
comparison$delta_mean_rpn
#> [1] -4.8
```

The mean RPN falls and the two worked risks drop to lower levels. That
is the before-and-after a reviewer is looking for: not that the study
had no risks, but that the risks were found, ranked, and brought down.

## Feeding the readiness score

The risk register does not live in isolation.
[`risk_register_to_evidence()`](https://r4sub.github.io/r4subrisk/reference/risk_register_to_evidence.md)
turns it into R4SUB evidence rows, which flow into the Risk pillar of
the Submission Confidence Index alongside quality, trace, and usability
evidence. The FMEA work you do here shows up directly in the
submission’s readiness score.
