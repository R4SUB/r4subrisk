# Changelog

## r4subrisk (development version)

- Add
  [`reconcile_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/reconcile_risk_registers.md),
  an item-level audit of what changed between two versions of a risk
  register (a prior vs current draft, or two assessors’ registers). It
  aligns rows by `risk_id` and labels each risk `added`, `removed`,
  `changed`, or `unchanged`; for changed risks it reports which fields
  differ (description, category, probability, impact, detectability,
  owner, mitigation, status) as a tidy `field_changes` audit, with prior
  and current values side by side. RPN movement is read from each
  register’s own stored `rpn` (nothing is recomputed or invented); it is
  a purely structural diff. This complements
  [`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md)
  (trajectory over many milestones) with a line-by-line view of a single
  pair.
- Fix the documented return value of
  [`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md):
  `rpn_changes` is the tibble of all risks common to both registers
  (unchanged ones included, with a zero delta), not only those whose RPN
  changed.
- Add
  [`risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.md),
  a Pareto (“vital few”) prioritization of a register. It ranks risks by
  their stored RPN (recomputing nothing), reports each risk’s share and
  the running cumulative share of total RPN, and flags the smallest top
  run of risks that together carry a caller-supplied cumulative share
  (`cum_cutoff`, default 0.8, the usual Pareto 80 percent). The cutoff
  is a reporting convention over cumulative RPN share, not a risk,
  severity, or RPN threshold.
  [`plot_risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/plot_risk_pareto.md)
  draws the classic bar-plus-cumulative-line Pareto chart when `ggplot2`
  is available.
- Add
  [`write_risk_register()`](https://r4sub.github.io/r4subrisk/reference/write_risk_register.md)
  and
  [`read_risk_register()`](https://r4sub.github.io/r4subrisk/reference/read_risk_register.md)
  for a risk register import/export round-trip. Registers export to CSV
  (or xlsx when `writexl` / `readxl` are installed) as a faithful,
  human-readable snapshot. On import the file is rebuilt through
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
  so the canonical schema, RPN, and risk-level classification are
  recomputed from the assessed scores rather than trusted from the file;
  a write -\> read cycle reproduces an equivalent register.
- Add
  [`risk_trend()`](https://r4sub.github.io/r4subrisk/reference/risk_trend.md),
  which turns a named, ordered list of risk registers (one per
  submission milestone) into a tidy burndown tibble: per milestone the
  total RPN, mean RPN, number of risks, and open-critical count. The
  stored RPN and risk levels are reused as-is, so no scoring is
  recomputed.
  [`plot_risk_trend()`](https://r4sub.github.io/r4subrisk/reference/plot_risk_trend.md)
  draws the burndown as a ggplot line chart when `ggplot2` is available.
- Import `%||%` from rlang so the package works on its declared minimum
  R version (R \>= 4.2); the operator only became part of base R in
  4.4.0.
- Fix the “Risk Management” vignette: the mitigation example now uses
  the `probability`, `impact`, and `detectability` columns that
  [`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md)
  actually reads (previously `*_residual` columns were silently ignored,
  so the before/after comparison showed no change), and the
  configuration example no longer references the non-existent
  `required_origins` field.

## r4subrisk 0.3.1

- Metadata and documentation polish for CRAN submission; no user-facing
  changes.

## r4subrisk 0.3.0

- Add
  [`risk_monte_carlo()`](https://r4sub.github.io/r4subrisk/reference/risk_monte_carlo.md),
  which puts uncertainty on the FMEA scores. An RPN is built from three
  integer judgements, so a single number hides how confident the
  assessment is. The simulation samples each probability, impact, and
  detectability around its assessed value and reports the distribution
  of RPN, per risk and for the register as a whole, including a 90%
  interval and the probability that each risk is actually critical. The
  random state is set and restored per call when a `seed` is given, so
  results are reproducible without disturbing the caller’s RNG.

## r4subrisk 0.2.0

- Add
  [`risk_reduction_summary()`](https://r4sub.github.io/r4subrisk/reference/risk_reduction_summary.md),
  which quantifies the risk removed between two registers: total and
  mean RPN reduction, percentage reduction, and counts of risks reduced,
  downgraded, added, and resolved.
- Add
  [`plot_risk_heatmap()`](https://r4sub.github.io/r4subrisk/reference/plot_risk_heatmap.md)
  and
  [`risk_heatmap_data()`](https://r4sub.github.io/r4subrisk/reference/risk_heatmap_data.md)
  for the standard FMEA probability by impact heatmap, with an optional
  interactive `plotly` version.
- Add a pluggable risk-model interface:
  [`risk_model()`](https://r4sub.github.io/r4subrisk/reference/risk_model.md)
  plus the pre-built
  [`risk_model_fmea()`](https://r4sub.github.io/r4subrisk/reference/risk_models.md),
  [`risk_model_probability_impact()`](https://r4sub.github.io/r4subrisk/reference/risk_models.md),
  and
  [`risk_model_ich_q9()`](https://r4sub.github.io/r4subrisk/reference/risk_models.md).
  [`apply_risk_model()`](https://r4sub.github.io/r4subrisk/reference/apply_risk_model.md)
  scores any set of risks to a normalized 0-100 value so alternative
  models feed the same Risk pillar.
- Add vignette: “Case study: FMEA risk on a pilot submission”, a worked
  before-and-after mitigation walkthrough using the example risk
  register from `r4subdata`.
- Clarified the package DESCRIPTION: “R4SUB” expands to “Ready for
  Submission” (previously “R for Regulatory Submission”, inconsistent
  with the rest of the ecosystem).

## r4subrisk 0.1.1

- Add vignette: “Risk Management with r4subrisk” covering
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
  [`compute_risk_scores()`](https://r4sub.github.io/r4subrisk/reference/compute_risk_scores.md),
  [`apply_mitigations()`](https://r4sub.github.io/r4subrisk/reference/apply_mitigations.md),
  [`compare_risk_registers()`](https://r4sub.github.io/r4subrisk/reference/compare_risk_registers.md),
  and
  [`evidence_to_risks()`](https://r4sub.github.io/r4subrisk/reference/evidence_to_risks.md)
  integration.

## r4subrisk 0.1.0

CRAN release: 2026-03-03

- Initial CRAN release.
