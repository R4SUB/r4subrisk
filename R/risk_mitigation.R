#' Update Risk Mitigation Status
#'
#' Applies mitigation updates to a risk register. Allows updating
#' probability, impact, detectability, status, and mitigation notes
#' for specific risks.
#'
#' @param risk_register A `risk_register` from [create_risk_register()].
#' @param updates A data.frame with at minimum column `risk_id`, plus any
#'   columns to update: `probability`, `impact`, `detectability`,
#'   `mitigation`, `status`.
#' @param config A `risk_config` from [risk_config_default()].
#'
#' @return An updated `risk_register` with recomputed RPN and risk levels.
#'
#' @examples
#' risks <- data.frame(
#'   risk_id = c("R001", "R002"),
#'   description = c("Missing vars", "Bad derivation"),
#'   probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
#' )
#' rr <- create_risk_register(risks)
#'
#' updates <- data.frame(
#'   risk_id     = "R001",
#'   probability = 2,
#'   mitigation  = "Added validation check",
#'   status      = "mitigated"
#' )
#' rr2 <- apply_mitigations(rr, updates)
#' rr2
#'
#' @export
apply_mitigations <- function(risk_register,
                              updates,
                              config = risk_config_default()) {
  if (!inherits(risk_register, "risk_register")) {
    cli::cli_abort("{.arg risk_register} must be a {.cls risk_register} object.")
  }
  if (!is.data.frame(updates)) {
    cli::cli_abort("{.arg updates} must be a data.frame.")
  }

  names(updates) <- tolower(trimws(names(updates)))

  if (!"risk_id" %in% names(updates)) {
    cli::cli_abort("{.arg updates} must contain a {.val risk_id} column.")
  }

  rr <- as.data.frame(risk_register)
  updatable <- c("probability", "impact", "detectability", "mitigation", "status")

  unknown_ids <- setdiff(updates$risk_id, rr$risk_id)
  if (length(unknown_ids) > 0L) {
    warning(
      "Skipping unknown risk_id(s): ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(updates))) {
    rid <- updates$risk_id[i]
    row_idx <- which(rr$risk_id == rid)
    if (length(row_idx) == 0L) next

    for (col in intersect(updatable, names(updates))) {
      val <- updates[[col]][i]
      if (!is.na(val)) {
        rr[[col]][row_idx] <- val
      }
    }
  }

  # Validate updated scales
  validate_fmea_scale(rr$probability, "probability")
  validate_fmea_scale(rr$impact, "impact")
  validate_fmea_scale(rr$detectability, "detectability")

  # Recompute RPN and risk level
  rr$rpn <- rr$probability * rr$impact * rr$detectability
  rr$risk_level <- vapply(
    rr$rpn,
    function(r) classify_rpn(r, bands = config$rpn_bands),
    character(1)
  )

  result <- tibble::as_tibble(rr)
  structure(result, class = c("risk_register", class(result)))
}


#' Compare Risk Registers (Trend Analysis)
#'
#' Compares two risk register snapshots and reports changes in RPN,
#' new risks, resolved risks, and risk level transitions.
#'
#' @param before A `risk_register` (earlier snapshot).
#' @param after A `risk_register` (later snapshot).
#'
#' @return A list with:
#'   - `rpn_changes`: tibble of risks with changed RPN
#'   - `new_risks`: risk_ids present in `after` but not `before`
#'   - `resolved_risks`: risk_ids present in `before` but not `after`
#'   - `level_transitions`: tibble of risk level changes
#'   - `delta_mean_rpn`: change in mean RPN
#'
#' @examples
#' r1 <- data.frame(
#'   risk_id = c("R001", "R002"),
#'   description = c("Missing vars", "Bad derivation"),
#'   probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
#' )
#' r2 <- data.frame(
#'   risk_id = c("R001", "R003"),
#'   description = c("Missing vars", "New issue"),
#'   probability = c(2, 3), impact = c(5, 3), detectability = c(2, 2)
#' )
#' rr1 <- create_risk_register(r1)
#' rr2 <- create_risk_register(r2)
#' compare_risk_registers(rr1, rr2)
#'
#' @export
compare_risk_registers <- function(before, after) {
  if (!inherits(before, "risk_register") || !inherits(after, "risk_register")) {
    cli::cli_abort("Both {.arg before} and {.arg after} must be {.cls risk_register} objects.")
  }

  ids_before <- before$risk_id
  ids_after  <- after$risk_id

  new_risks      <- setdiff(ids_after, ids_before)
  resolved_risks <- setdiff(ids_before, ids_after)
  common_ids     <- intersect(ids_before, ids_after)

  # RPN changes for common risks
  if (length(common_ids) > 0L) {
    before_sub <- before[match(common_ids, before$risk_id), ]
    after_sub  <- after[match(common_ids, after$risk_id), ]

    rpn_changes <- tibble::tibble(
      risk_id    = common_ids,
      rpn_before = before_sub$rpn,
      rpn_after  = after_sub$rpn,
      rpn_delta  = after_sub$rpn - before_sub$rpn
    )

    level_transitions <- tibble::tibble(
      risk_id      = common_ids,
      level_before = before_sub$risk_level,
      level_after  = after_sub$risk_level,
      changed      = before_sub$risk_level != after_sub$risk_level
    )
  } else {
    rpn_changes <- tibble::tibble(
      risk_id = character(0), rpn_before = numeric(0),
      rpn_after = numeric(0), rpn_delta = numeric(0)
    )
    level_transitions <- tibble::tibble(
      risk_id = character(0), level_before = character(0),
      level_after = character(0), changed = logical(0)
    )
  }

  # Delta mean RPN
  mean_before <- if (nrow(before) > 0L) mean(before$rpn, na.rm = TRUE) else 0
  mean_after  <- if (nrow(after) > 0L) mean(after$rpn, na.rm = TRUE) else 0

  list(
    rpn_changes       = rpn_changes,
    new_risks         = new_risks,
    resolved_risks    = resolved_risks,
    level_transitions = level_transitions,
    delta_mean_rpn    = round(mean_after - mean_before, 1)
  )
}
