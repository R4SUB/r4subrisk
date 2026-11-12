#' Pareto Prioritization of a Risk Register
#'
#' Orders the risks in a register from highest to lowest Risk Priority Number
#' and reports, for each risk, its share of the total RPN and the running
#' cumulative share. This is the classic Pareto ("vital few") view: it surfaces
#' the small set of risks that together drive most of the register's RPN, so a
#' programme can direct mitigation effort where it removes the most risk first.
#'
#' @details
#' No scoring is recomputed. The already-stored `rpn` from
#' [create_risk_register()] is used as-is, and risks are ranked by it (ties are
#' broken by `risk_id` so the order is deterministic). `rpn_share` is each
#' risk's RPN divided by the register total; `cum_share` is the running sum of
#' those shares down the ranking.
#'
#' A risk is flagged in `vital_few` when it falls inside the shortest run of
#' top-ranked risks whose cumulative share first reaches `cum_cutoff`. The
#' cutoff is a reporting convention supplied by the caller (its default, `0.8`,
#' is the usual Pareto 80 percent) and is applied to the cumulative RPN share
#' only; it is not a risk, severity, or RPN threshold and does not change any
#' risk level. When the total RPN is zero (for example an empty register) no
#' risk is flagged.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param cum_cutoff The cumulative RPN share that defines the vital few, a
#'   single number in `(0, 1]`. Defaults to `0.8`.
#'
#' @return A tibble of class `"risk_pareto"`, ordered by descending RPN, with
#'   columns `rank`, `risk_id`, `description`, `category`, `rpn`, `risk_level`,
#'   `rpn_share`, `cum_rpn`, `cum_share`, and `vital_few`. The cutoff used, the
#'   register total RPN, and the number of vital-few risks are attached as the
#'   attributes `cum_cutoff`, `total_rpn`, and `n_vital_few`.
#'
#' @seealso [plot_risk_pareto()] for the optional ggplot Pareto chart,
#'   [compute_risk_scores()] for register-level aggregates.
#'
#' @examples
#' rr <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002", "R003", "R004"),
#'   description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
#'                     "Inconsistent define.xml", "Label mismatches"),
#'   probability   = c(4, 3, 2, 1),
#'   impact        = c(5, 4, 3, 2),
#'   detectability = c(2, 3, 4, 1)
#' ))
#' risk_pareto(rr)
#'
#' @export
risk_pareto <- function(risk_register, cum_cutoff = 0.8) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }
  if (!is.numeric(cum_cutoff) || length(cum_cutoff) != 1L || is.na(cum_cutoff) ||
      cum_cutoff <= 0 || cum_cutoff > 1) {
    cli::cli_abort("{.arg cum_cutoff} must be a single number in (0, 1].")
  }

  rr <- risk_register
  n  <- nrow(rr)

  empty <- tibble::tibble(
    rank        = integer(0),
    risk_id     = character(0),
    description = character(0),
    category    = character(0),
    rpn         = numeric(0),
    risk_level  = character(0),
    rpn_share   = numeric(0),
    cum_rpn     = numeric(0),
    cum_share   = numeric(0),
    vital_few   = logical(0)
  )

  if (n == 0L) {
    return(structure(
      empty,
      class       = c("risk_pareto", class(empty)),
      cum_cutoff  = cum_cutoff,
      total_rpn   = 0,
      n_vital_few = 0L
    ))
  }

  total_rpn <- sum(rr$rpn, na.rm = TRUE)

  # Rank by RPN (highest first); break ties on risk_id for a stable order.
  ord <- order(-rr$rpn, rr$risk_id)
  rr  <- rr[ord, , drop = FALSE]

  rpn <- rr$rpn
  cum_rpn <- cumsum(rpn)

  if (total_rpn > 0) {
    rpn_share <- rpn / total_rpn
    cum_share <- cum_rpn / total_rpn
    # Vital few: shortest top run whose cumulative share reaches the cutoff.
    k <- which(cum_share >= cum_cutoff)[1]
    vital_few <- seq_len(n) <= k
  } else {
    rpn_share <- rep(0, n)
    cum_share <- rep(0, n)
    vital_few <- rep(FALSE, n)
  }

  out <- tibble::tibble(
    rank        = seq_len(n),
    risk_id     = as.character(rr$risk_id),
    description = as.character(rr$description),
    category    = as.character(rr$category),
    rpn         = rpn,
    risk_level  = as.character(rr$risk_level),
    rpn_share   = round(rpn_share, 4),
    cum_rpn     = cum_rpn,
    cum_share   = round(cum_share, 4),
    vital_few   = vital_few
  )

  structure(
    out,
    class       = c("risk_pareto", class(out)),
    cum_cutoff  = cum_cutoff,
    total_rpn   = total_rpn,
    n_vital_few = as.integer(sum(vital_few))
  )
}


#' Print a Risk Pareto Table
#' @param x A `risk_pareto` object.
#' @param ... Ignored.
#' @export
print.risk_pareto <- function(x, ...) {
  cutoff    <- attr(x, "cum_cutoff")
  total_rpn <- attr(x, "total_rpn")
  n_vital   <- attr(x, "n_vital_few")
  n         <- nrow(x)

  cli::cli_alert_info("Risk Pareto: {n} risk(s), total RPN {.val {total_rpn}}")
  if (n > 0L && total_rpn > 0) {
    pct <- round(cutoff * 100)
    share <- round(100 * n_vital / n)
    cli::cli_alert_info(
      "  Vital few: {n_vital} risk(s) ({share}% of risks) carry \\
       >= {pct}% of total RPN"
    )
  }
  NextMethod()
  invisible(x)
}


#' Plot a Risk Pareto Chart
#'
#' Draws the classic Pareto chart for a risk register: a bar per risk, ordered
#' by descending RPN, with the running cumulative RPN share overlaid as a line.
#' The vital-few bars are highlighted and the cutoff is drawn as a reference
#' line. Reporting only; it recomputes no scores.
#'
#' @param risk_register A `risk_register` from [create_risk_register()], or a
#'   tibble returned by [risk_pareto()].
#' @param cum_cutoff The cumulative RPN share that defines the vital few,
#'   forwarded to [risk_pareto()] when `risk_register` is a register. Defaults
#'   to `0.8`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [risk_pareto()].
#'
#' @examples
#' \dontrun{
#' rr <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002", "R003", "R004"),
#'   description   = c("a", "b", "c", "d"),
#'   probability   = c(4, 3, 2, 1),
#'   impact        = c(5, 4, 3, 2),
#'   detectability = c(2, 3, 4, 1)
#' ))
#' plot_risk_pareto(rr)
#' }
#'
#' @export
plot_risk_pareto <- function(risk_register, cum_cutoff = 0.8) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required for {.fn plot_risk_pareto}. \\
       Install it with {.code install.packages(\"ggplot2\")}."
    )
  }

  dat <- if (inherits(risk_register, "risk_pareto")) {
    risk_register
  } else {
    risk_pareto(risk_register, cum_cutoff = cum_cutoff)
  }

  if (nrow(dat) == 0L) {
    cli::cli_abort("Cannot plot an empty risk register.")
  }

  cutoff <- attr(dat, "cum_cutoff") %||% cum_cutoff

  dat$risk_id <- factor(dat$risk_id, levels = dat$risk_id)
  max_rpn <- max(dat$rpn, na.rm = TRUE)
  # Scale the 0-1 cumulative share onto the RPN axis for the overlaid line.
  dat$cum_scaled <- dat$cum_share * max_rpn

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$risk_id)) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$rpn, fill = .data$vital_few)
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$cum_scaled, group = 1),
      colour = "#333333", linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$cum_scaled), colour = "#333333", size = 2
    ) +
    ggplot2::geom_hline(
      yintercept = cutoff * max_rpn, linetype = "dashed", colour = "grey50"
    ) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "#d7191c", `FALSE` = "#2c7bb6"),
      labels = c(`TRUE` = "Vital few", `FALSE` = "Trivial many"),
      name   = NULL
    ) +
    ggplot2::scale_y_continuous(
      name = "RPN",
      sec.axis = ggplot2::sec_axis(
        ~ . / max_rpn, name = "Cumulative RPN share",
        labels = function(b) paste0(round(b * 100), "%")
      )
    ) +
    ggplot2::labs(title = "Risk Pareto", x = "Risk") +
    ggplot2::theme_minimal()
}
