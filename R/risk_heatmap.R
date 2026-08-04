#' Build Risk Heatmap Data
#'
#' Aggregates a risk register into the probability by impact grid that an FMEA
#' heatmap is drawn from. Every cell of the 5 by 5 grid is returned, including
#' empty ones, so the plotted heatmap has no gaps.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param config A `risk_config` from [risk_config_default()], used to assign a
#'   zone to each cell from the cell's mean RPN.
#'
#' @return A tibble with one row per grid cell: `probability`, `impact`, `n`
#'   (risks in the cell), `mean_rpn`, `max_rpn`, and `zone` (the risk level of
#'   the cell's mean RPN, `NA` for empty cells).
#'
#' @examples
#' rr <- create_risk_register(data.frame(
#'   risk_id = c("R001", "R002"),
#'   description = c("a", "b"),
#'   probability = c(4, 2), impact = c(5, 3), detectability = c(2, 3)
#' ))
#' risk_heatmap_data(rr)
#'
#' @export
risk_heatmap_data <- function(risk_register, config = risk_config_default()) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }

  grid <- expand.grid(probability = 1:5, impact = 1:5)

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    p <- grid$probability[i]
    m <- grid$impact[i]
    hit <- risk_register$probability == p & risk_register$impact == m
    n <- sum(hit, na.rm = TRUE)

    if (n == 0L) {
      mean_rpn <- NA_real_
      max_rpn  <- NA_real_
      zone     <- NA_character_
    } else {
      mean_rpn <- round(mean(risk_register$rpn[hit], na.rm = TRUE), 1)
      max_rpn  <- max(risk_register$rpn[hit], na.rm = TRUE)
      zone     <- classify_rpn(mean_rpn, bands = config$rpn_bands)
    }

    tibble::tibble(
      probability = as.integer(p),
      impact      = as.integer(m),
      n           = as.integer(n),
      mean_rpn    = mean_rpn,
      max_rpn     = max_rpn,
      zone        = zone
    )
  })

  dplyr::bind_rows(rows)
}


#' Plot an FMEA Risk Heatmap
#'
#' Draws the standard FMEA probability by impact heatmap. Cells are coloured by
#' the chosen metric and labelled with the number of risks that fall in them.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param metric The cell fill, one of `"mean_rpn"`, `"max_rpn"`, or `"n"`.
#' @param config A `risk_config` from [risk_config_default()].
#' @param interactive If `TRUE`, return a `plotly` widget instead of a `ggplot`.
#'   Requires the `plotly` package.
#'
#' @return A `ggplot` object, or a `plotly` object when `interactive = TRUE`.
#'
#' @examples
#' \dontrun{
#' risks <- data.frame(
#'   risk_id       = c("R001", "R002", "R003"),
#'   description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
#'                     "Inconsistent define.xml"),
#'   category      = c("data_quality", "traceability", "documentation"),
#'   probability   = c(4, 3, 2),
#'   impact        = c(5, 4, 3),
#'   detectability = c(2, 3, 4)
#' )
#' rr <- create_risk_register(risks)
#' plot_risk_heatmap(rr)
#' }
#'
#' @export
plot_risk_heatmap <- function(risk_register,
                              metric = c("mean_rpn", "max_rpn", "n"),
                              config = risk_config_default(),
                              interactive = FALSE) {
  metric <- match.arg(metric)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required for {.fn plot_risk_heatmap}. \\
       Install it with {.code install.packages(\"ggplot2\")}."
    )
  }

  dat <- risk_heatmap_data(risk_register, config = config)
  dat$fill_value <- dat[[metric]]
  dat$label <- ifelse(dat$n > 0L, as.character(dat$n), "")

  metric_label <- c(mean_rpn = "Mean RPN", max_rpn = "Max RPN",
                    n = "Risk count")[[metric]]

  p <- ggplot2::ggplot(
    dat, ggplot2::aes(x = .data$probability, y = .data$impact)
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data$fill_value), colour = "grey85"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3.5) +
    ggplot2::scale_x_continuous(breaks = 1:5, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = 1:5, expand = c(0, 0)) +
    ggplot2::scale_fill_gradient(
      low = "#2c7bb6", high = "#d7191c", na.value = "grey95",
      name = metric_label
    ) +
    ggplot2::labs(
      title = "FMEA Risk Heatmap",
      x = "Probability", y = "Impact"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg plotly} is required for the interactive heatmap."
      )
    }
    return(plotly::ggplotly(p))
  }

  p
}
