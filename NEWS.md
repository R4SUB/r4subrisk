# r4subrisk (development version)

- Add `risk_pareto()`, a Pareto ("vital few") prioritization of a register. It
  ranks risks by their stored RPN (recomputing nothing), reports each risk's
  share and the running cumulative share of total RPN, and flags the smallest
  top run of risks that together carry a caller-supplied cumulative share
  (`cum_cutoff`, default 0.8, the usual Pareto 80 percent). The cutoff is a
  reporting convention over cumulative RPN share, not a risk, severity, or RPN
  threshold. `plot_risk_pareto()` draws the classic bar-plus-cumulative-line
  Pareto chart when `ggplot2` is available.
- Add `write_risk_register()` and `read_risk_register()` for a risk register
  import/export round-trip. Registers export to CSV (or xlsx when `writexl` /
  `readxl` are installed) as a faithful, human-readable snapshot. On import the
  file is rebuilt through `create_risk_register()`, so the canonical schema,
  RPN, and risk-level classification are recomputed from the assessed scores
  rather than trusted from the file; a write -> read cycle reproduces an
  equivalent register.
- Add `risk_trend()`, which turns a named, ordered list of risk registers (one
  per submission milestone) into a tidy burndown tibble: per milestone the total
  RPN, mean RPN, number of risks, and open-critical count. The stored RPN and
  risk levels are reused as-is, so no scoring is recomputed. `plot_risk_trend()`
  draws the burndown as a ggplot line chart when `ggplot2` is available.
- Import `%||%` from rlang so the package works on its declared minimum R
  version (R >= 4.2); the operator only became part of base R in 4.4.0.
- Fix the "Risk Management" vignette: the mitigation example now uses the
  `probability`, `impact`, and `detectability` columns that `apply_mitigations()`
  actually reads (previously `*_residual` columns were silently ignored, so the
  before/after comparison showed no change), and the configuration example no
  longer references the non-existent `required_origins` field.

# r4subrisk 0.3.1

- Metadata and documentation polish for CRAN submission; no user-facing changes.

# r4subrisk 0.3.0

- Add `risk_monte_carlo()`, which puts uncertainty on the FMEA scores. An RPN is
  built from three integer judgements, so a single number hides how confident
  the assessment is. The simulation samples each probability, impact, and
  detectability around its assessed value and reports the distribution of RPN,
  per risk and for the register as a whole, including a 90% interval and the
  probability that each risk is actually critical. The random state is set and
  restored per call when a `seed` is given, so results are reproducible without
  disturbing the caller's RNG.

# r4subrisk 0.2.0

- Add `risk_reduction_summary()`, which quantifies the risk removed between two
  registers: total and mean RPN reduction, percentage reduction, and counts of
  risks reduced, downgraded, added, and resolved.
- Add `plot_risk_heatmap()` and `risk_heatmap_data()` for the standard FMEA
  probability by impact heatmap, with an optional interactive `plotly` version.
- Add a pluggable risk-model interface: `risk_model()` plus the pre-built
  `risk_model_fmea()`, `risk_model_probability_impact()`, and
  `risk_model_ich_q9()`. `apply_risk_model()` scores any set of risks to a
  normalized 0-100 value so alternative models feed the same Risk pillar.
- Add vignette: "Case study: FMEA risk on a pilot submission", a worked
  before-and-after mitigation walkthrough using the example risk register from
  `r4subdata`.
- Clarified the package DESCRIPTION: "R4SUB" expands to "Ready for Submission"
  (previously "R for Regulatory Submission", inconsistent with the rest of the
  ecosystem).

# r4subrisk 0.1.1

- Add vignette: "Risk Management with r4subrisk" covering `create_risk_register()`,
  `compute_risk_scores()`, `apply_mitigations()`, `compare_risk_registers()`,
  and `evidence_to_risks()` integration.

# r4subrisk 0.1.0

- Initial CRAN release.
