# r4subrisk

<!-- badges: start -->
[![R-CMD-check](https://github.com/R4SUB/r4subrisk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subrisk/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/r4subrisk)](https://CRAN.R-project.org/package=r4subrisk)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/r4subrisk)](https://CRAN.R-project.org/package=r4subrisk)
[![r-universe](https://r4sub.r-universe.dev/badges/r4subrisk)](https://r4sub.r-universe.dev/r4subrisk)
<!-- badges: end -->

**r4subrisk** is the risk quantification engine in the R4SUB ecosystem. It uses an **FMEA-inspired framework** (Probability × Impact × Detectability) to quantify submission risk, build risk registers, track mitigations, and emit standardized R4SUB evidence rows.

> What are the key risks to submission readiness, how severe are they, and are they being addressed?

## Installation

```r
install.packages("r4subrisk")
```

Development version:

```r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subrisk"))
```

## FMEA Risk Model

Each risk is scored on three dimensions (1–5 scale):

- **Probability**: likelihood the issue persists to submission
- **Impact**: regulatory consequence if unresolved
- **Detectability**: difficulty of catching the issue (1 = easy, 5 = hard)

**RPN** (Risk Priority Number) = Probability × Impact × Detectability (range 1–125)

| RPN | Level | Action |
|---|---|---|
| 80–125 | `critical` | Immediate action required |
| 40–79 | `high` | Must resolve before submission |
| 15–39 | `medium` | Plan mitigation |
| 1–14 | `low` | Monitor |

## Quick Start

```r
library(r4subcore)
library(r4subrisk)

# Build a risk register manually
risks <- data.frame(
  risk_id       = c("R001", "R002"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations"),
  category      = c("data_quality", "traceability"),
  probability   = c(4, 3),
  impact        = c(5, 4),
  detectability = c(2, 3)
)

rr     <- create_risk_register(risks)
scores <- compute_risk_scores(rr)

# Or derive risks from an evidence table
risk_items <- evidence_to_risks(evidence)
rr <- create_risk_register(risk_items)

# Emit r4subcore-compatible evidence rows
ctx <- r4sub_run_context(study_id = "ABC123", environment = "DEV")
ev  <- risk_register_to_evidence(rr, ctx = ctx)
```

## Key Functions

| Function | Purpose |
|---|---|
| `risk_config_default()` | FMEA scales, RPN bands, severity mappings |
| `classify_rpn()` | Classify an RPN value into a risk level |
| `create_risk_register()` | Build a risk register with RPN + levels |
| `evidence_to_risks()` | Derive risk items from r4subcore evidence |
| `compute_risk_scores()` | Aggregate risk metrics (mean/max RPN, distribution) |
| `risk_indicator_summary()` | Summary indicator table |
| `risk_register_to_evidence()` | Emit r4subcore-compatible evidence rows |
| `apply_mitigations()` | Update risks with mitigations, recompute RPN |
| `compare_risk_registers()` | Trend analysis between snapshots |

## Design Principles

- **FMEA-grounded:** industry-standard risk methodology
- **Evidence-first:** all risk items emit r4subcore evidence rows
- **Mitigation-aware:** track and measure risk reduction over time
- **Transparent:** every RPN is decomposable into P × I × D

## Maintained by

R4SUB is part of the open-source work of [TechWorksLab](https://techworkslab.com) - clinical programming and regulatory submissions. Maintainer: Pawan Rama Mali.

## License

MIT
