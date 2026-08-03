#' Define a Risk Scoring Model
#'
#' Wraps a scoring rule so the engine can score risks with something other than
#' the default FMEA product. A model turns a data.frame of risks into a raw
#' score, a normalized 0--100 risk score, and a qualitative level. Use the
#' pre-built models or supply your own `score` function.
#'
#' @param name A short model name.
#' @param score A function taking a data.frame of risks and returning a numeric
#'   vector of raw scores, one per row.
#' @param max_raw The raw score at maximum risk, used to normalize to 0--100.
#' @param bands A named list of `c(lower, upper)` boundaries over the raw score,
#'   ordered from highest risk to lowest. Evaluated in order; first match wins.
#' @param required Character vector of columns the `score` function needs.
#'
#' @return An object of class `"risk_model"`.
#'
#' @examples
#' m <- risk_model(
#'   name = "impact_only",
#'   score = function(df) df$impact,
#'   max_raw = 5,
#'   bands = list(high = c(4, 5), medium = c(3, 3), low = c(1, 2)),
#'   required = "impact"
#' )
#' apply_risk_model(make_test_risks(), m)
#'
#' @export
risk_model <- function(name, score, max_raw, bands, required = character()) {
  if (!is.character(name) || length(name) != 1L) {
    cli::cli_abort("{.arg name} must be a single string.")
  }
  if (!is.function(score)) {
    cli::cli_abort("{.arg score} must be a function.")
  }
  if (!is.numeric(max_raw) || length(max_raw) != 1L || max_raw <= 0) {
    cli::cli_abort("{.arg max_raw} must be a single positive number.")
  }
  if (!is.list(bands) || length(bands) == 0L) {
    cli::cli_abort("{.arg bands} must be a non-empty named list.")
  }

  structure(
    list(
      name     = name,
      score    = score,
      max_raw  = max_raw,
      bands    = bands,
      required = required
    ),
    class = "risk_model"
  )
}


#' Pre-Built Risk Models
#'
#' Ready-made [risk_model()] objects for the common frameworks. `risk_model_fmea()`
#' is the default three-factor FMEA product. `risk_model_probability_impact()` is
#' a two-factor probability times impact matrix. `risk_model_ich_q9()` is a
#' qualitative ICH Q9 style ranking that collapses probability and impact into
#' acceptable, tolerable, and unacceptable levels.
#'
#' @param config A `risk_config` from [risk_config_default()], used by the FMEA
#'   model to pick up custom RPN bands.
#'
#' @return A `risk_model` object.
#'
#' @examples
#' apply_risk_model(make_test_risks(), risk_model_probability_impact())
#' apply_risk_model(make_test_risks(), risk_model_ich_q9())
#'
#' @name risk_models
NULL

#' @rdname risk_models
#' @export
risk_model_fmea <- function(config = risk_config_default()) {
  risk_model(
    name     = "fmea",
    score    = function(df) df$probability * df$impact * df$detectability,
    max_raw  = 125,
    bands    = config$rpn_bands,
    required = c("probability", "impact", "detectability")
  )
}

#' @rdname risk_models
#' @export
risk_model_probability_impact <- function() {
  risk_model(
    name     = "probability_impact",
    score    = function(df) df$probability * df$impact,
    max_raw  = 25,
    bands    = list(
      critical = c(20, 25),
      high     = c(12, 19),
      medium   = c(6, 11),
      low      = c(1, 5)
    ),
    required = c("probability", "impact")
  )
}

#' @rdname risk_models
#' @export
risk_model_ich_q9 <- function() {
  # Collapse a 1-5 scale into three qualitative steps (1 = low, 3 = high).
  to_three <- function(x) {
    ifelse(x <= 2, 1L, ifelse(x <= 3, 2L, 3L))
  }
  risk_model(
    name     = "ich_q9",
    score    = function(df) to_three(df$probability) * to_three(df$impact),
    max_raw  = 9,
    bands    = list(
      unacceptable = c(6, 9),
      tolerable    = c(3, 5),
      acceptable   = c(1, 2)
    ),
    required = c("probability", "impact")
  )
}


#' Score Risks with a Model
#'
#' Applies a [risk_model()] to a set of risks and returns per-risk raw scores,
#' normalized 0--100 risk scores, and qualitative levels. Any model that
#' produces a 0--100 risk score plugs into the same Risk pillar computation.
#'
#' @param risks A data.frame or `risk_register` with the columns the model
#'   requires. Must include `risk_id`.
#' @param model A `risk_model`. Defaults to [risk_model_fmea()].
#'
#' @return A tibble with columns `risk_id`, `raw_score`, `risk_score` (0--100,
#'   higher is more risk), `risk_level`, and `model`.
#'
#' @examples
#' apply_risk_model(make_test_risks(), risk_model_fmea())
#'
#' @export
apply_risk_model <- function(risks, model = risk_model_fmea()) {
  if (!inherits(model, "risk_model")) {
    cli::cli_abort("{.arg model} must be a {.cls risk_model} object.")
  }
  if (!is.data.frame(risks)) {
    cli::cli_abort("{.arg risks} must be a data.frame or risk_register.")
  }

  df <- as.data.frame(risks)
  names(df) <- tolower(trimws(names(df)))

  if (!"risk_id" %in% names(df)) {
    cli::cli_abort("{.arg risks} must contain a {.val risk_id} column.")
  }
  missing_cols <- setdiff(model$required, names(df))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Model {.val {model$name}} needs column(s): {.val {missing_cols}}."
    )
  }

  raw <- as.numeric(model$score(df))
  risk_score <- round(pmin(100, pmax(0, raw / model$max_raw * 100)), 1)
  level <- vapply(raw, function(r) classify_raw(r, model$bands), character(1))

  tibble::tibble(
    risk_id    = as.character(df$risk_id),
    raw_score  = raw,
    risk_score = risk_score,
    risk_level = level,
    model      = model$name
  )
}


#' Overall Normalized Risk from a Scored Set
#'
#' Reduces the per-risk output of [apply_risk_model()] to a single normalized
#' 0--100 risk figure (the mean risk score). This is the value the Risk pillar
#' consumes, and it is model-agnostic by construction.
#'
#' @param scored A tibble from [apply_risk_model()].
#'
#' @return A single number in `[0, 100]`.
#'
#' @examples
#' scored <- apply_risk_model(make_test_risks(), risk_model_ich_q9())
#' risk_model_overall(scored)
#'
#' @export
risk_model_overall <- function(scored) {
  if (!is.data.frame(scored) || !"risk_score" %in% names(scored)) {
    cli::cli_abort("{.arg scored} must be output from {.fn apply_risk_model}.")
  }
  if (nrow(scored) == 0L) return(0)
  round(mean(scored$risk_score, na.rm = TRUE), 1)
}


#' Print a Risk Model
#' @param x A `risk_model` object.
#' @param ... Ignored.
#' @export
print.risk_model <- function(x, ...) {
  cli::cli_alert_info("Risk model: {.val {x$name}}")
  cli::cli_alert_info("  Needs columns: {.val {x$required}}")
  cli::cli_alert_info("  Levels: {.val {names(x$bands)}}")
  invisible(x)
}


# Classify a raw score against a model's bands (same first-match rule as
# classify_rpn, but over an arbitrary raw range).
classify_raw <- function(raw, bands) {
  for (nm in names(bands)) {
    rng <- bands[[nm]]
    if (!is.na(raw) && raw >= rng[1] && raw <= rng[2]) {
      return(nm)
    }
  }
  "unclassified"
}
