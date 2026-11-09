#' Risk Burndown Across Submission Milestones
#'
#' Summarizes a sequence of risk registers, one per submission milestone, into a
#' tidy burndown tibble. Where [compare_risk_registers()] and
#' [risk_reduction_summary()] tell the before/after story of a single mitigation
#' step, `risk_trend()` tracks the whole trajectory, so a programme can show
#' reviewers that risk is falling milestone over milestone rather than at a
#' single point in time.
#'
#' @details
#' The registers are treated as an ordered sequence: the list order is the
#' milestone order (earliest first). Names, when supplied, are used as milestone
#' labels; an unnamed or blank entry falls back to `milestone_<n>`. Each register
#' keeps its own already-computed `rpn` and `risk_level` (from
#' [create_risk_register()] / [classify_rpn()]), so no scoring is recomputed
#' here. The open-critical count is the number of risks whose level is
#' `"critical"` and whose `status` is `"open"`; an empty register contributes a
#' total and mean RPN of 0.
#'
#' @param registers A named (or otherwise ordered) list of `risk_register`
#'   objects from [create_risk_register()], one per milestone, earliest first.
#' @param config A `risk_config` from [risk_config_default()]. Reserved for
#'   configurable classification; the registers' stored levels are used as-is.
#'
#' @return A tibble of class `"risk_trend"` with one row per milestone and the
#'   columns `milestone`, `milestone_index`, `n_risks`, `total_rpn`, `mean_rpn`,
#'   and `n_open_critical`.
#'
#' @seealso [plot_risk_trend()] for the optional ggplot burndown,
#'   [risk_reduction_summary()] for a pairwise before/after summary.
#'
#' @examples
#' m1 <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002", "R003"),
#'   description   = c("Missing vars", "Bad derivation", "Define gap"),
#'   probability   = c(4, 5, 2),
#'   impact        = c(5, 5, 3),
#'   detectability = c(4, 4, 4)
#' ))
#' m2 <- apply_mitigations(
#'   m1, data.frame(risk_id = c("R001", "R002"), probability = c(2, 2))
#' )
#' m3 <- apply_mitigations(
#'   m2, data.frame(risk_id = "R003", detectability = 1)
#' )
#' risk_trend(list(baseline = m1, interim = m2, final = m3))
#'
#' @export
risk_trend <- function(registers, config = risk_config_default()) {
  if (!is.list(registers) || inherits(registers, "risk_register")) {
    cli::cli_abort(
      "{.arg registers} must be a list of {.cls risk_register} objects, \\
       one per milestone."
    )
  }
  if (length(registers) == 0L) {
    cli::cli_abort("{.arg registers} must contain at least one register.")
  }

  not_reg <- !vapply(registers, inherits, logical(1), "risk_register")
  if (any(not_reg)) {
    cli::cli_abort(
      "Every element of {.arg registers} must be a {.cls risk_register}; \\
       element(s) {.val {which(not_reg)}} are not."
    )
  }

  labels <- names(registers)
  if (is.null(labels)) labels <- rep(NA_character_, length(registers))
  blank <- is.na(labels) | !nzchar(trimws(labels))
  labels[blank] <- paste0("milestone_", seq_along(registers)[blank])

  rows <- lapply(seq_along(registers), function(i) {
    rr <- registers[[i]]
    n <- nrow(rr)
    tibble::tibble(
      milestone       = labels[[i]],
      milestone_index = as.integer(i),
      n_risks         = as.integer(n),
      total_rpn       = if (n > 0L) sum(rr$rpn, na.rm = TRUE) else 0,
      mean_rpn        = if (n > 0L) round(mean(rr$rpn, na.rm = TRUE), 1) else 0,
      n_open_critical = as.integer(sum(
        rr$risk_level == "critical" & rr$status == "open",
        na.rm = TRUE
      ))
    )
  })

  out <- dplyr::bind_rows(rows)
  structure(out, class = c("risk_trend", class(tibble::tibble())))
}


#' Plot a Risk Burndown
#'
#' Draws the milestone-over-milestone burndown produced by [risk_trend()] as a
#' line-and-point chart. Reporting only; it recomputes no scores.
#'
#' @param registers A named list of `risk_register` objects, as passed to
#'   [risk_trend()], or the tibble returned by [risk_trend()].
#' @param metric The series to plot, one of `"total_rpn"`, `"mean_rpn"`, or
#'   `"n_open_critical"`.
#' @param config A `risk_config` from [risk_config_default()], forwarded to
#'   [risk_trend()] when `registers` is a list.
#'
#' @return A `ggplot` object.
#'
#' @seealso [risk_trend()].
#'
#' @examples
#' \dontrun{
#' m1 <- create_risk_register(data.frame(
#'   risk_id = c("R001", "R002"), description = c("A", "B"),
#'   probability = c(4, 5), impact = c(5, 5), detectability = c(4, 4)
#' ))
#' m2 <- apply_mitigations(m1, data.frame(risk_id = c("R001", "R002"),
#'                                         probability = c(2, 2)))
#' plot_risk_trend(list(baseline = m1, interim = m2))
#' }
#'
#' @export
plot_risk_trend <- function(registers,
                            metric = c("total_rpn", "mean_rpn",
                                       "n_open_critical"),
                            config = risk_config_default()) {
  metric <- match.arg(metric)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required for {.fn plot_risk_trend}. \\
       Install it with {.code install.packages(\"ggplot2\")}."
    )
  }

  dat <- if (inherits(registers, "risk_trend")) {
    registers
  } else {
    risk_trend(registers, config = config)
  }

  dat$milestone <- factor(dat$milestone, levels = dat$milestone)
  dat$y <- dat[[metric]]

  metric_label <- c(total_rpn = "Total RPN", mean_rpn = "Mean RPN",
                    n_open_critical = "Open critical risks")[[metric]]

  ggplot2::ggplot(
    dat, ggplot2::aes(x = .data$milestone, y = .data$y, group = 1)
  ) +
    ggplot2::geom_line(colour = "#2c7bb6", linewidth = 0.8) +
    ggplot2::geom_point(colour = "#2c7bb6", size = 2.5) +
    ggplot2::labs(
      title = "Risk Burndown",
      x = "Milestone", y = metric_label
    ) +
    ggplot2::theme_minimal()
}
