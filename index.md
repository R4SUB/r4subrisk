# r4subrisk

**r4subrisk** is the risk quantification engine in the R4SUB ecosystem.

It uses an **FMEA-inspired framework** (Probability x Impact x
Detectability) to quantify submission risk, build risk registers, track
mitigations, and emit standardized R4SUB evidence rows.

It answers the question:

> What are the key risks to submission readiness, how severe are they,
> and are they being addressed?

## Core Concepts

### FMEA Risk Model

Each risk is scored on three dimensions (1–5 scale):

- **Probability**: likelihood the issue persists to submission
- **Impact**: regulatory consequence if unresolved
- **Detectability**: difficulty of catching the issue (1=easy, 5=hard)

**RPN** (Risk Priority Number) = Probability x Impact x Detectability
(range 1–125)

### RPN Bands

| RPN    | Level      | Interpretation                 |
|--------|------------|--------------------------------|
| 80–125 | `critical` | Immediate action required      |
| 40–79  | `high`     | Must resolve before submission |
| 15–39  | `medium`   | Plan mitigation                |
| 1–14   | `low`      | Monitor                        |

## Installation

``` r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subrisk"))
```

## Quick Start

``` r
library(r4subcore)
library(r4subrisk)

# From a manual risk register
risks <- data.frame(
  risk_id       = c("R001", "R002"),
  description   = c("Missing SDTM variables", "Unmapped ADaM derivations"),
  category      = c("data_quality", "traceability"),
  probability   = c(4, 3),
  impact        = c(5, 4),
  detectability = c(2, 3)
)
rr <- create_risk_register(risks)
rr

# Or derive risks automatically from evidence
risk_items <- evidence_to_risks(evidence)
rr <- create_risk_register(risk_items)

# Compute scores and emit evidence
scores <- compute_risk_scores(rr)
ctx <- r4sub_run_context(study_id = "ABC123", environment = "DEV")
ev <- risk_register_to_evidence(rr, ctx = ctx)
```

## Core Functions

| Function                                                                                                  | Purpose                                             |
|-----------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md)             | FMEA scales, RPN bands, severity mappings           |
| [`classify_rpn()`](https://r4sub.github.io/r4subrisk/reference/classify_rpn.md)                           | Classify an RPN value into a risk level             |
| [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md)           | Build a risk register with RPN + levels             |
| [`evidence_to_risks()`](https://r4sub.github.io/r4subrisk/reference/evidence_to_risks.md)                 | Derive risk items from r4subcore evidence           |
| [`compute_risk_scores()`](https://r4sub.github.io/r4subrisk/reference/compute_risk_scores.md)             | Aggregate risk metrics (mean/max RPN, distribution) |
| [`risk_indicator_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_indicator_summary.md)       | Summary indicator table                             |
| [`risk_register_to_evidence()`](https://r4sub.github.io/r4subrisk/reference/risk_register_to_evidence.md) | Emit r4subcore-compatible evidence rows             |
| [`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md)                 | Update risks with mitigations, recompute RPN        |
| [`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md)       | Trend analysis between snapshots                    |

## Design Principles

- **FMEA-grounded**: industry-standard risk methodology
- **Evidence-first**: all risk items emit r4subcore evidence rows
- **Mitigation-aware**: track and measure risk reduction over time
- **Transparent**: every RPN is decomposable into P x I x D

## License

MIT
