#' Compute Risk Indicator Summary
#'
#' Computes summary risk indicators from a risk register, similar to
#' `r4subtrace::trace_indicator_scores()`.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#'
#' @return A tibble with columns: `indicator`, `value`, `description`.
#'
#' @examples
#' risks <- data.frame(
#'   risk_id = c("R001", "R002", "R003"),
#'   description = c("Missing vars", "Bad derivation", "Label mismatch"),
#'   probability = c(4, 2, 1), impact = c(5, 3, 2),
#'   detectability = c(2, 3, 1)
#' )
#' rr <- create_risk_register(risks)
#' risk_indicator_summary(rr)
#'
#' @export
risk_indicator_summary <- function(risk_register) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }

  rr <- risk_register
  n <- nrow(rr)

  if (n == 0L) {
    return(tibble::tibble(
      indicator   = c("RISK_TOTAL_COUNT", "RISK_OPEN_COUNT",
                       "RISK_CRITICAL_COUNT", "RISK_HIGH_COUNT",
                       "RISK_MEAN_RPN", "RISK_MAX_RPN",
                       "RISK_OVERALL_SCORE"),
      value       = rep(0, 7),
      description = c(
        "Total number of identified risks",
        "Number of open (unresolved) risks",
        "Number of critical-level risks",
        "Number of high-level risks",
        "Mean Risk Priority Number across all risks",
        "Maximum Risk Priority Number",
        "Overall risk score (0=none, 1=maximum)"
      )
    ))
  }

  n_open     <- sum(rr$status == "open", na.rm = TRUE)
  n_critical <- sum(rr$risk_level == "critical", na.rm = TRUE)
  n_high     <- sum(rr$risk_level == "high", na.rm = TRUE)
  mean_rpn   <- round(mean(rr$rpn, na.rm = TRUE), 1)
  max_rpn    <- max(rr$rpn, na.rm = TRUE)
  overall    <- round(min(1, mean_rpn / 125), 4)

  tibble::tibble(
    indicator = c(
      "RISK_TOTAL_COUNT",
      "RISK_OPEN_COUNT",
      "RISK_CRITICAL_COUNT",
      "RISK_HIGH_COUNT",
      "RISK_MEAN_RPN",
      "RISK_MAX_RPN",
      "RISK_OVERALL_SCORE"
    ),
    value = c(
      as.numeric(n),
      as.numeric(n_open),
      as.numeric(n_critical),
      as.numeric(n_high),
      mean_rpn,
      as.numeric(max_rpn),
      overall
    ),
    description = c(
      "Total number of identified risks",
      "Number of open (unresolved) risks",
      "Number of critical-level risks",
      "Number of high-level risks",
      "Mean Risk Priority Number across all risks",
      "Maximum Risk Priority Number",
      "Overall risk score (0=none, 1=maximum)"
    )
  )
}
