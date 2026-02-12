#' Convert Risk Register to R4SUB Evidence
#'
#' Emits evidence rows compatible with `r4subcore::validate_evidence()` for
#' each risk item in the register, plus aggregate risk metric rows.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param ctx An `r4sub_run_context` from `r4subcore::r4sub_run_context()`.
#' @param source_name Character; the name of the evidence source.
#' @param source_version Character or `NULL`; version of the source.
#'
#' @return A data.frame of evidence rows passing `r4subcore::validate_evidence()`.
#'
#' @examples
#' \dontrun{
#' library(r4subcore)
#' ctx <- r4sub_run_context(study_id = "TEST001", environment = "DEV")
#' risks <- data.frame(
#'   risk_id = "R001", description = "Missing vars",
#'   probability = 4, impact = 5, detectability = 2
#' )
#' rr <- create_risk_register(risks)
#' ev <- risk_register_to_evidence(rr, ctx = ctx)
#' }
#'
#' @export
risk_register_to_evidence <- function(risk_register,
                                      ctx,
                                      source_name    = "r4subrisk",
                                      source_version = NULL) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }

  rr <- risk_register
  evidence_rows <- list()

  if (nrow(rr) > 0L) {
    # Map risk_level to severity
    level_to_severity <- c(
      critical     = "critical",
      high         = "high",
      medium       = "medium",
      low          = "low",
      unclassified = "medium"
    )

    # Map risk_level to result
    level_to_result <- c(
      critical     = "fail",
      high         = "fail",
      medium       = "warn",
      low          = "pass",
      unclassified = "warn"
    )

    severities <- unname(level_to_severity[rr$risk_level])
    severities[is.na(severities)] <- "medium"
    results <- unname(level_to_result[rr$risk_level])
    results[is.na(results)] <- "warn"

    payloads <- vapply(seq_len(nrow(rr)), function(i) {
      r4subcore::json_safely(list(
        risk_id       = rr$risk_id[i],
        probability   = rr$probability[i],
        impact        = rr$impact[i],
        detectability = rr$detectability[i],
        rpn           = rr$rpn[i],
        risk_level    = rr$risk_level[i],
        category      = rr$category[i],
        status        = rr$status[i]
      ))
    }, character(1))

    per_risk_ev <- data.frame(
      asset_type       = "other",
      asset_id         = rr$risk_id,
      source_name      = source_name,
      source_version   = source_version %||% NA_character_,
      indicator_id     = "RISK_ITEM",
      indicator_name   = "Individual risk assessment",
      indicator_domain = "risk",
      severity         = severities,
      result           = results,
      metric_value     = as.numeric(rr$rpn),
      metric_unit      = "RPN",
      message          = paste0(
        rr$risk_id, ": ", rr$description,
        " (RPN=", rr$rpn, ", level=", rr$risk_level, ")"
      ),
      location         = paste0(rr$category, ":", rr$risk_id),
      evidence_payload = payloads,
      stringsAsFactors = FALSE
    )
    evidence_rows <- c(evidence_rows, list(per_risk_ev))

    # Aggregate metric: overall risk score
    scores <- compute_risk_scores(rr)

    agg_ev <- data.frame(
      asset_type       = "other",
      asset_id         = "RISK_AGGREGATE",
      source_name      = source_name,
      source_version   = source_version %||% NA_character_,
      indicator_id     = "RISK_OVERALL_SCORE",
      indicator_name   = "Overall risk score",
      indicator_domain = "risk",
      severity         = if (scores$overall_risk_score >= 0.6) "high" else
        if (scores$overall_risk_score >= 0.3) "medium" else "low",
      result           = if (scores$overall_risk_score >= 0.6) "fail" else
        if (scores$overall_risk_score >= 0.3) "warn" else "pass",
      metric_value     = scores$overall_risk_score,
      metric_unit      = "score_0_1",
      message          = paste0(
        "Overall risk score: ", scores$overall_risk_score,
        " (mean RPN: ", scores$mean_rpn, ", max RPN: ", scores$max_rpn, ")"
      ),
      location         = NA_character_,
      evidence_payload = r4subcore::json_safely(list(
        mean_rpn  = scores$mean_rpn,
        max_rpn   = scores$max_rpn,
        n_risks   = scores$n_risks
      )),
      stringsAsFactors = FALSE
    )
    evidence_rows <- c(evidence_rows, list(agg_ev))
  }

  if (length(evidence_rows) == 0L) {
    cli::cli_alert_warning("No evidence rows generated from risk register.")
    ev <- empty_risk_evidence(source_name, source_version)
    return(r4subcore::as_evidence(ev, ctx = ctx))
  }

  ev <- do.call(rbind, evidence_rows)
  r4subcore::as_evidence(ev, ctx = ctx)
}


#' Create empty risk evidence data.frame
#' @noRd
empty_risk_evidence <- function(source_name = "r4subrisk",
                                source_version = NULL) {
  data.frame(
    asset_type       = character(0),
    asset_id         = character(0),
    source_name      = character(0),
    source_version   = character(0),
    indicator_id     = character(0),
    indicator_name   = character(0),
    indicator_domain = character(0),
    severity         = character(0),
    result           = character(0),
    metric_value     = numeric(0),
    metric_unit      = character(0),
    message          = character(0),
    location         = character(0),
    evidence_payload = character(0),
    stringsAsFactors = FALSE
  )
}
