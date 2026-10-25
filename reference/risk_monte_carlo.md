# Monte Carlo Uncertainty on Risk Priority Numbers

An FMEA scores each risk with single integer values for probability,
impact, and detectability, but those assessments are judgements with
uncertainty. This simulation treats each score as uncertain, draws many
possible registers, and reports the resulting distribution of Risk
Priority Numbers, both per risk and for the register as a whole. It
turns a single RPN into a range and a probability of being critical,
which is what a risk review actually wants to know.

## Usage

``` r
risk_monte_carlo(
  register,
  n = 10000,
  spread = 1,
  config = risk_config_default(),
  seed = NULL
)
```

## Arguments

- register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md),
  or a data.frame with `risk_id`, `probability`, `impact`, and
  `detectability`.

- n:

  Number of Monte Carlo iterations. Default `10000`.

- spread:

  Standard deviation of the per-component uncertainty, in scale points.
  Default `1`. Use `0` for a deterministic result.

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md),
  used for the critical threshold.

- seed:

  Optional integer seed. When supplied the random state is set for the
  call and restored on exit, so the result is reproducible without
  disturbing the caller's RNG stream.

## Value

A list of class `"risk_monte_carlo"` with `n`, `spread`,
`crit_threshold`, `point_total` (the deterministic register RPN),
`total` (named numeric summary of the total RPN distribution),
`total_draws` (the simulated totals, for plotting), and `per_risk` (a
data.frame with `risk_id`, `rpn`, `mc_mean`, `mc_sd`, `p05`, `p50`,
`p95`, and `prob_critical`).

## Details

For each draw, every FMEA component is sampled from a normal
distribution centred on its assessed value with standard deviation
`spread` (in scale points), then rounded and clamped to the 1–5 scale.
`spread = 0` reproduces the deterministic RPN. The per-draw RPN is
`probability * impact * detectability`, and the register total is their
sum. The critical threshold is the lower bound of the top RPN band in
`config`.

## Examples

``` r
risks <- data.frame(
  risk_id = c("R001", "R002", "R003"),
  description = c("Missing SDTM vars", "Unmapped derivations", "Define drift"),
  probability = c(4, 3, 2), impact = c(5, 4, 3), detectability = c(2, 3, 4)
)
rr <- suppressMessages(create_risk_register(risks))
mc <- risk_monte_carlo(rr, n = 2000, seed = 1)
mc$total
#>  mean    sd   p05   p50   p95   min   max 
#>  96.1  32.0  50.0  92.0 156.0  21.0 230.0 
mc$per_risk
#>   risk_id rpn mc_mean mc_sd p05 p50 p95 prob_critical
#> 1    R002  36    35.1  19.8   9  32  75         0.048
#> 2    R001  40    36.9  19.2  12  34  75         0.031
#> 3    R003  24    24.1  15.5   6  20  50         0.011
```
