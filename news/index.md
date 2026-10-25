# Changelog

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
