#' Create a Risk Register
#'
#' Builds a risk register from a user-supplied data.frame of identified risks.
#' Validates required columns and fills defaults.
#'
#' @param risks A data.frame with at minimum columns `risk_id` and
#'   `description`. Optional columns: `category`, `probability`, `impact`,
#'   `detectability`, `owner`, `mitigation`, `status`.
#' @param config A `risk_config` from [risk_config_default()].
#'
#' @return A tibble of class `"risk_register"` with standardized columns and
#'   computed RPN values.
#'
#' @examples
#' risks <- data.frame(
#'   risk_id     = c("R001", "R002", "R003"),
#'   description = c("Missing SDTM variables", "Unmapped ADaM derivations",
#'                    "Inconsistent define.xml"),
#'   category    = c("data_quality", "traceability", "documentation"),
#'   probability = c(4, 3, 2),
#'   impact      = c(5, 4, 3),
#'   detectability = c(2, 3, 4)
#' )
#' rr <- create_risk_register(risks)
#' rr
#'
#' @export
create_risk_register <- function(risks, config = risk_config_default()) {
  if (!is.data.frame(risks)) {
    cli::cli_abort("{.arg risks} must be a data.frame, not {.cls {class(risks)}}.")
  }

  # Canonicalize column names
  names(risks) <- tolower(trimws(names(risks)))

  # Required columns
  required <- c("risk_id", "description")
  missing_cols <- setdiff(required, names(risks))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Risk register is missing required column(s): {.val {missing_cols}}."
    )
  }

  # Fill optional columns with defaults
  nr <- nrow(risks)
  if (!"category" %in% names(risks))       risks$category       <- rep("general", nr)
  if (!"probability" %in% names(risks))    risks$probability    <- rep(3, nr)
  if (!"impact" %in% names(risks))         risks$impact         <- rep(3, nr)
  if (!"detectability" %in% names(risks))  risks$detectability  <- rep(config$default_detectability, nr)
  if (!"owner" %in% names(risks))          risks$owner          <- rep(NA_character_, nr)
  if (!"mitigation" %in% names(risks))     risks$mitigation     <- rep(NA_character_, nr)
  if (!"status" %in% names(risks))         risks$status         <- rep("open", nr)

  # Coerce types
  risks$risk_id        <- as.character(risks$risk_id)
  risks$description    <- as.character(risks$description)
  risks$category       <- as.character(risks$category)
  risks$probability    <- as.numeric(risks$probability)
  risks$impact         <- as.numeric(risks$impact)
  risks$detectability  <- as.numeric(risks$detectability)
  risks$owner          <- as.character(risks$owner)
  risks$mitigation     <- as.character(risks$mitigation)
  risks$status         <- as.character(risks$status)

  # Validate FMEA scales (1-5)
  validate_fmea_scale(risks$probability, "probability")
  validate_fmea_scale(risks$impact, "impact")
  validate_fmea_scale(risks$detectability, "detectability")

  # Check for duplicate risk_ids
  dup_ids <- risks$risk_id[duplicated(risks$risk_id)]
  if (length(dup_ids) > 0L) {
    cli::cli_abort("Duplicate risk_id(s) found: {.val {unique(dup_ids)}}.")
  }

  # Compute RPN
  risks$rpn <- risks$probability * risks$impact * risks$detectability

  # Classify risk level
  risks$risk_level <- vapply(
    risks$rpn,
    function(r) classify_rpn(r, bands = config$rpn_bands),
    character(1)
  )

  result <- tibble::as_tibble(risks)
  structure(result, class = c("risk_register", class(result)))
}


#' Print Risk Register
#' @param x A `risk_register` object.
#' @param ... Ignored.
#' @export
print.risk_register <- function(x, ...) {
  n <- nrow(x)
  n_open <- sum(x$status == "open", na.rm = TRUE)
  n_crit <- sum(x$risk_level == "critical", na.rm = TRUE)
  n_high <- sum(x$risk_level == "high", na.rm = TRUE)
  mean_rpn <- round(mean(x$rpn, na.rm = TRUE), 1)

  cli::cli_alert_info("Risk Register: {n} risk(s), {n_open} open")
  cli::cli_alert_info(
    "  Critical: {n_crit}, High: {n_high}, Mean RPN: {mean_rpn}"
  )
  NextMethod()
}


#' Validate FMEA scale values (1-5)
#' @noRd
validate_fmea_scale <- function(x, name) {
  valid <- !is.na(x)
  if (any(valid)) {
    out_of_range <- x[valid] < 1 | x[valid] > 5
    if (any(out_of_range)) {
      cli::cli_abort("{.arg {name}} values must be between 1 and 5.")
    }
  }
}
