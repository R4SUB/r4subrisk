#' Summarize Risk Reduction Between Two Registers
#'
#' Quantifies how much risk was removed between an earlier and a later risk
#' register, so a mitigation effort can be reported as a single before/after
#' story. Regulatory reviewers expect to see that identified risks were acted
#' on, not just listed.
#'
#' @details
#' Reduction is measured on the risks common to both registers. Total RPN is the
#' sum of Risk Priority Numbers, mean RPN is their average, and the percentage
#' reduction is relative to the before total. A risk is counted as `downgraded`
#' when its risk level moves to a lower band (for example `critical` to `high`).
#' Risks that appear only in `after` are reported as `n_added`, and risks that
#' disappear are reported as `n_resolved`.
#'
#' @param before A `risk_register` (earlier snapshot).
#' @param after A `risk_register` (later snapshot, usually the result of
#'   [apply_mitigations()]).
#' @param config A `risk_config` from [risk_config_default()], used to order the
#'   risk-level bands when deciding what counts as a downgrade.
#'
#' @return A list of class `"risk_reduction"` with overall figures and a
#'   `per_risk` tibble of the common risks. Fields include `total_rpn_before`,
#'   `total_rpn_after`, `total_rpn_reduction`, `pct_reduction`,
#'   `mean_rpn_before`, `mean_rpn_after`, `n_reduced`, `n_increased`,
#'   `n_unchanged`, `n_downgraded`, `n_added`, `n_resolved`, and `per_risk`.
#'
#' @examples
#' risks <- data.frame(
#'   risk_id = c("R001", "R002"),
#'   description = c("Missing vars", "Bad derivation"),
#'   probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
#' )
#' rr <- create_risk_register(risks)
#' rr2 <- apply_mitigations(rr, data.frame(risk_id = "R001", probability = 1))
#' risk_reduction_summary(rr, rr2)
#'
#' @export
risk_reduction_summary <- function(before, after,
                                   config = risk_config_default()) {
  if (!inherits(before, "risk_register") || !inherits(after, "risk_register")) {
    cli::cli_abort(
      "Both {.arg before} and {.arg after} must be {.cls risk_register} objects."
    )
  }

  common <- intersect(before$risk_id, after$risk_id)
  n_added    <- length(setdiff(after$risk_id, before$risk_id))
  n_resolved <- length(setdiff(before$risk_id, after$risk_id))

  # Order the level bands from lowest to highest so we can tell a downgrade
  # (less risk) from an upgrade. Unknown levels sort last.
  level_order <- rev(names(config$rpn_bands))
  rank_of <- function(lv) {
    r <- match(lv, level_order)
    ifelse(is.na(r), length(level_order) + 1L, r)
  }

  if (length(common) > 0L) {
    b <- before[match(common, before$risk_id), , drop = FALSE]
    a <- after[match(common, after$risk_id), , drop = FALSE]

    per_risk <- tibble::tibble(
      risk_id      = common,
      rpn_before   = b$rpn,
      rpn_after    = a$rpn,
      rpn_delta    = a$rpn - b$rpn,
      level_before = b$risk_level,
      level_after  = a$risk_level,
      downgraded   = rank_of(a$risk_level) < rank_of(b$risk_level)
    )

    n_reduced   <- sum(per_risk$rpn_delta < 0)
    n_increased <- sum(per_risk$rpn_delta > 0)
    n_unchanged <- sum(per_risk$rpn_delta == 0)
    n_downgraded <- sum(per_risk$downgraded)
  } else {
    per_risk <- tibble::tibble(
      risk_id = character(0), rpn_before = numeric(0), rpn_after = numeric(0),
      rpn_delta = numeric(0), level_before = character(0),
      level_after = character(0), downgraded = logical(0)
    )
    n_reduced <- n_increased <- n_unchanged <- n_downgraded <- 0L
  }

  total_before <- sum(per_risk$rpn_before)
  total_after  <- sum(per_risk$rpn_after)
  reduction    <- total_before - total_after
  pct <- if (total_before > 0) round(reduction / total_before * 100, 1) else 0

  structure(
    list(
      total_rpn_before    = total_before,
      total_rpn_after     = total_after,
      total_rpn_reduction = reduction,
      pct_reduction       = pct,
      mean_rpn_before     = if (length(common) > 0L) round(mean(per_risk$rpn_before), 1) else 0,
      mean_rpn_after      = if (length(common) > 0L) round(mean(per_risk$rpn_after), 1) else 0,
      n_common            = length(common),
      n_reduced           = as.integer(n_reduced),
      n_increased         = as.integer(n_increased),
      n_unchanged         = as.integer(n_unchanged),
      n_downgraded        = as.integer(n_downgraded),
      n_added             = as.integer(n_added),
      n_resolved          = as.integer(n_resolved),
      per_risk            = per_risk
    ),
    class = "risk_reduction"
  )
}


#' Print a Risk Reduction Summary
#' @param x A `risk_reduction` object.
#' @param ... Ignored.
#' @export
print.risk_reduction <- function(x, ...) {
  cli::cli_alert_info("Risk Reduction Summary")
  cli::cli_alert_info(
    "  Total RPN: {x$total_rpn_before} -> {x$total_rpn_after} \\
     ({x$total_rpn_reduction} down, {x$pct_reduction}%)"
  )
  cli::cli_alert_info(
    "  Mean RPN: {x$mean_rpn_before} -> {x$mean_rpn_after}"
  )
  cli::cli_alert_info(
    "  Risks reduced: {x$n_reduced}, downgraded a level: {x$n_downgraded}"
  )
  if (x$n_added > 0L || x$n_resolved > 0L) {
    cli::cli_alert_info(
      "  New risks: {x$n_added}, resolved risks: {x$n_resolved}"
    )
  }
  invisible(x)
}
