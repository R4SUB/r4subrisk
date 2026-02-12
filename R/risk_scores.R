#' Compute Risk Scores from a Risk Register
#'
#' Computes aggregate risk metrics from a risk register, including mean RPN,
#' risk distribution, and overall risk score normalized to 0--1.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param config A `risk_config` from [risk_config_default()].
#'
#' @return A list of class `"risk_scores"` with:
#'   - `overall_risk_score`: 0--1 (0 = no risk, 1 = maximum risk)
#'   - `mean_rpn`: average RPN across all risks
#'   - `max_rpn`: highest individual RPN
#'   - `n_risks`: total risk count
#'   - `risk_distribution`: tibble of counts by risk_level
#'   - `category_summary`: tibble of mean RPN by category
#'
#' @examples
#' risks <- data.frame(
#'   risk_id = c("R001", "R002"),
#'   description = c("Missing vars", "Bad derivation"),
#'   probability = c(4, 2), impact = c(5, 3), detectability = c(2, 3)
#' )
#' rr <- create_risk_register(risks)
#' compute_risk_scores(rr)
#'
#' @export
compute_risk_scores <- function(risk_register, config = risk_config_default()) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }

  rr <- risk_register
  n <- nrow(rr)

  if (n == 0L) {
    return(structure(
      list(
        overall_risk_score = 0,
        mean_rpn           = 0,
        max_rpn            = 0,
        n_risks            = 0L,
        risk_distribution  = tibble::tibble(risk_level = character(0), n = integer(0)),
        category_summary   = tibble::tibble(category = character(0),
                                            n = integer(0), mean_rpn = numeric(0))
      ),
      class = "risk_scores"
    ))
  }

  mean_rpn <- mean(rr$rpn, na.rm = TRUE)
  max_rpn  <- max(rr$rpn, na.rm = TRUE)

  # Normalize to 0-1: RPN max is 125 (5*5*5)
  overall_risk_score <- min(1, mean_rpn / 125)

  # Distribution by risk level
  level_tab <- as.data.frame(table(rr$risk_level), stringsAsFactors = FALSE)
  names(level_tab) <- c("risk_level", "n")
  level_tab$n <- as.integer(level_tab$n)

  # Category summary
  cat_key <- rr$category
  cat_groups <- split(seq_len(n), cat_key)
  cat_rows <- lapply(names(cat_groups), function(g) {
    idx <- cat_groups[[g]]
    tibble::tibble(
      category = g,
      n        = as.integer(length(idx)),
      mean_rpn = round(mean(rr$rpn[idx], na.rm = TRUE), 1)
    )
  })
  category_summary <- dplyr::bind_rows(cat_rows)

  structure(
    list(
      overall_risk_score = round(overall_risk_score, 4),
      mean_rpn           = round(mean_rpn, 1),
      max_rpn            = max_rpn,
      n_risks            = n,
      risk_distribution  = tibble::as_tibble(level_tab),
      category_summary   = category_summary
    ),
    class = "risk_scores"
  )
}


#' Print Risk Scores
#' @param x A `risk_scores` object.
#' @param ... Ignored.
#' @export
print.risk_scores <- function(x, ...) {
  cli::cli_alert_info("Risk Score Summary")
  cli::cli_alert_info("  Overall risk score: {.val {x$overall_risk_score}} (0=none, 1=max)")
  cli::cli_alert_info("  Mean RPN: {.val {x$mean_rpn}}, Max RPN: {.val {x$max_rpn}}")
  cli::cli_alert_info("  Total risks: {.val {x$n_risks}}")

  if (nrow(x$risk_distribution) > 0L) {
    for (i in seq_len(nrow(x$risk_distribution))) {
      cli::cli_alert_info(
        "    {x$risk_distribution$risk_level[i]}: {x$risk_distribution$n[i]}"
      )
    }
  }
  invisible(x)
}
