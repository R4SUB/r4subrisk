## Submission

This is an update of r4subrisk from 0.1.0 (on CRAN) to 0.2.0, a feature release
for the R4SUB (Ready for Submission) ecosystem. Highlights:

* `risk_reduction_summary()` quantifying risk removed between two registers.
* `plot_risk_heatmap()` and `risk_heatmap_data()` for the FMEA heatmap.
* A pluggable risk-model interface (`risk_model()` and pre-built models).

See NEWS.md for the complete list.

## Test environments

* local: Windows 11 x64, R 4.5.x
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

r4subrisk is imported by the r4sub meta-package. Changes are additive and
existing interfaces are unchanged.
