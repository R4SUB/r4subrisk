#' Default Risk Configuration
#'
#' Returns configuration for risk assessment including FMEA scale definitions,
#' RPN thresholds, and risk level classification bands.
#'
#' @param rpn_bands Named list of RPN band boundaries `c(lower, upper)`.
#'   Evaluated in order; first match wins.
#' @param evidence_severity_to_probability Named numeric vector mapping
#'   evidence severity to probability scores (1--5 scale).
#' @param evidence_severity_to_impact Named numeric vector mapping
#'   evidence severity to impact scores (1--5 scale).
#' @param default_detectability Default detectability score (1--5) when not
#'   explicitly provided. Lower = more detectable.
#'
#' @return A list of class `"risk_config"` with elements:
#'   `rpn_bands`, `evidence_severity_to_probability`,
#'   `evidence_severity_to_impact`, `default_detectability`.
#'
#' @details
#' The FMEA-inspired risk model uses three dimensions:
#' - **Probability** (1--5): likelihood of the issue occurring/persisting
#' - **Impact** (1--5): severity of consequence if unresolved
#' - **Detectability** (1--5): difficulty of detecting the issue (1 = easy, 5 = hard)
#'
#' **RPN** = Probability x Impact x Detectability (range 1--125)
#'
#' @examples
#' cfg <- risk_config_default()
#' cfg$rpn_bands
#'
#' @export
risk_config_default <- function(
    rpn_bands = list(
      critical = c(80, 125),
      high     = c(40, 79),
      medium   = c(15, 39),
      low      = c(1, 14)
    ),
    evidence_severity_to_probability = c(
      info     = 1,
      low      = 2,
      medium   = 3,
      high     = 4,
      critical = 5
    ),
    evidence_severity_to_impact = c(
      info     = 1,
      low      = 2,
      medium   = 3,
      high     = 4,
      critical = 5
    ),
    default_detectability = 3
) {
  # Validate
  if (!is.list(rpn_bands) || length(rpn_bands) == 0L) {
    cli::cli_abort("{.arg rpn_bands} must be a non-empty named list.")
  }
  for (nm in names(rpn_bands)) {
    b <- rpn_bands[[nm]]
    if (!is.numeric(b) || length(b) != 2L) {
      cli::cli_abort("RPN band {.val {nm}} must be a numeric vector of length 2.")
    }
  }

  if (!is.numeric(default_detectability) || length(default_detectability) != 1L ||
      default_detectability < 1 || default_detectability > 5) {
    cli::cli_abort("{.arg default_detectability} must be a number between 1 and 5.")
  }

  structure(
    list(
      rpn_bands                        = rpn_bands,
      evidence_severity_to_probability = evidence_severity_to_probability,
      evidence_severity_to_impact      = evidence_severity_to_impact,
      default_detectability            = default_detectability
    ),
    class = "risk_config"
  )
}


#' Classify RPN Value into Risk Level
#'
#' @param rpn Numeric RPN score (1--125).
#' @param bands Named list of band boundaries from [risk_config_default()].
#'
#' @return Character risk level name.
#'
#' @examples
#' classify_rpn(90)
#' classify_rpn(25)
#' classify_rpn(5)
#'
#' @export
classify_rpn <- function(rpn, bands = risk_config_default()$rpn_bands) {
  if (!is.numeric(rpn) || length(rpn) != 1L) {
    cli::cli_abort("{.arg rpn} must be a single numeric value.")
  }

  for (nm in names(bands)) {
    rng <- bands[[nm]]
    if (rpn >= rng[1] && rpn <= rng[2]) {
      return(nm)
    }
  }

  "unclassified"
}
