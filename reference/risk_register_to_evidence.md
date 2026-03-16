# Convert Risk Register to R4SUB Evidence

Emits evidence rows compatible with
[`r4subcore::validate_evidence()`](https://rdrr.io/pkg/r4subcore/man/validate_evidence.html)
for each risk item in the register, plus aggregate risk metric rows.

## Usage

``` r
risk_register_to_evidence(
  risk_register,
  ctx,
  source_name = "r4subrisk",
  source_version = NULL
)
```

## Arguments

- risk_register:

  A `risk_register` from
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.md).

- ctx:

  An `r4sub_run_context` from
  [`r4subcore::r4sub_run_context()`](https://rdrr.io/pkg/r4subcore/man/r4sub_run_context.html).

- source_name:

  Character; the name of the evidence source.

- source_version:

  Character or `NULL`; version of the source.

## Value

A data.frame of evidence rows passing
[`r4subcore::validate_evidence()`](https://rdrr.io/pkg/r4subcore/man/validate_evidence.html).

## Examples

``` r
if (FALSE) { # \dontrun{
library(r4subcore)
ctx <- r4sub_run_context(study_id = "TEST001", environment = "DEV")
risks <- data.frame(
  risk_id = "R001", description = "Missing vars",
  probability = 4, impact = 5, detectability = 2
)
rr <- create_risk_register(risks)
ev <- risk_register_to_evidence(rr, ctx = ctx)
} # }
```
