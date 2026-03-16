# Derive Risk Items from Evidence

Automatically generates risk items from an R4SUB evidence table. Each
failing or warning indicator becomes a potential risk, with probability
and impact inferred from evidence severity.

## Usage

``` r
evidence_to_risks(
  evidence,
  config = risk_config_default(),
  include_pass = FALSE
)
```

## Arguments

- evidence:

  A validated evidence data.frame (from `r4subcore`).

- config:

  A `risk_config` from
  [`risk_config_default()`](https://r4sub.github.io/r4subrisk/reference/risk_config_default.md).

- include_pass:

  Logical; if `TRUE`, passing indicators are also included as low-risk
  items. Default `FALSE`.

## Value

A tibble suitable for
[`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

## Details

The mapping from evidence to risk uses:

- `risk_id`: derived from `indicator_id` + `asset_id` via
  [`r4subcore::hash_id()`](https://rdrr.io/pkg/r4subcore/man/hash_id.html)

- `category`: mapped from `indicator_domain`

- `probability`: mapped from evidence `severity` via config

- `impact`: mapped from evidence `severity` via config

- `detectability`: uses `config$default_detectability`

Multiple evidence rows for the same indicator + asset are aggregated:
probability and impact use the maximum across rows.

## Examples

``` r
if (FALSE) { # \dontrun{
risk_items <- evidence_to_risks(evidence)
rr <- create_risk_register(risk_items)
} # }
```
