#' Reconcile Two Risk Registers
#'
#' Aligns two versions of a risk register by their risk identifier and reports,
#' item by item, exactly what changed between them: which risks were added,
#' removed, left unchanged, or changed, and for the changed risks, which fields
#' differ. Where [risk_trend()] tracks the whole trajectory over many
#' milestones and [risk_reduction_summary()] quantifies how much RPN was
#' removed, `reconcile_risk_registers()` gives the line-by-line audit of a
#' single pair, as a reviewer comparing a prior submission draft against the
#' current one, or two assessors' independent registers, would want to see.
#'
#' @details
#' The two registers are matched on `risk_id` (unique within a register, since
#' [create_risk_register()] rejects duplicates). Each risk in the union of the
#' two is assigned one `change_type`:
#' \itemize{
#'   \item `"added"` --- present in `current` but not in `prior`;
#'   \item `"removed"` --- present in `prior` but not in `current`;
#'   \item `"changed"` --- present in both, with at least one tracked field
#'     differing;
#'   \item `"unchanged"` --- present in both, with every tracked field equal.
#' }
#' Two values are treated as equal when both are `NA`, and as differing when one
#' is `NA` and the other is not. No scores, scales, bands, or thresholds are
#' invented or recomputed: this is a purely structural diff. RPN movement is
#' read from each register's own stored `rpn` (the value [create_risk_register()]
#' already computed), and `risk_level` is likewise read as stored; both are
#' reported as movement rather than as tracked change fields, because they are
#' derived from `probability`, `impact`, and `detectability` rather than edited
#' directly.
#'
#' @param prior A `risk_register` from [create_risk_register()]: the earlier or
#'   baseline version.
#' @param current A `risk_register` from [create_risk_register()]: the later or
#'   comparison version.
#' @param fields Character vector of column names whose differences define a
#'   `"changed"` risk. Defaults to the assessment and management fields
#'   `c("description", "category", "probability", "impact", "detectability",
#'   "owner", "mitigation", "status")`. Every name must be a column of both
#'   registers.
#' @param config A `risk_config` from [risk_config_default()]. Accepted for
#'   interface consistency with the other register functions; the stored RPN and
#'   levels are used as-is, so it currently influences nothing.
#'
#' @return A list of class `"risk_reconciliation"` with:
#'   \itemize{
#'     \item `per_risk`: a tibble with one row per risk in the union of the two
#'       registers and columns `risk_id`, `change_type`, `n_fields_changed`,
#'       `changed_fields` (comma-separated, `NA` when none), `rpn_prior`,
#'       `rpn_current`, `rpn_delta`, `level_prior`, and `level_current`;
#'     \item `field_changes`: a long tibble of every differing field on the
#'       changed risks, with columns `risk_id`, `field`, `value_prior`, and
#'       `value_current` (values rendered as character for a uniform audit
#'       column);
#'     \item scalar counts `n_prior`, `n_current`, `n_added`, `n_removed`,
#'       `n_changed`, `n_unchanged`;
#'     \item `total_rpn_prior`, `total_rpn_current`, `total_rpn_delta`, the
#'       register-level RPN movement over the stored values;
#'     \item `fields`, the tracked field names used.
#'   }
#'
#' @seealso [compare_risk_registers()] for a lighter RPN/level diff,
#'   [risk_reduction_summary()] for the before/after reduction magnitude,
#'   [risk_trend()] for the trajectory across many milestones.
#'
#' @examples
#' prior <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002", "R003"),
#'   description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
#'                     "Inconsistent define.xml"),
#'   probability   = c(4, 3, 2),
#'   impact        = c(5, 4, 3),
#'   detectability = c(2, 3, 4),
#'   status        = c("open", "open", "open")
#' ))
#' current <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002", "R004"),
#'   description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
#'                     "Late-breaking lab data"),
#'   probability   = c(2, 3, 3),
#'   impact        = c(5, 4, 4),
#'   detectability = c(2, 3, 2),
#'   status        = c("mitigated", "open", "open")
#' ))
#' rec <- reconcile_risk_registers(prior, current)
#' rec
#' rec$per_risk
#' rec$field_changes
#'
#' @export
reconcile_risk_registers <- function(prior, current,
                                     fields = c("description", "category",
                                                "probability", "impact",
                                                "detectability", "owner",
                                                "mitigation", "status"),
                                     config = risk_config_default()) {
  if (!inherits(prior, "risk_register") || !inherits(current, "risk_register")) {
    cli::cli_abort(
      "Both {.arg prior} and {.arg current} must be {.cls risk_register} objects."
    )
  }
  if (!is.character(fields) || length(fields) == 0L) {
    cli::cli_abort("{.arg fields} must be a non-empty character vector.")
  }
  fields <- unique(fields)
  missing_prior   <- setdiff(fields, names(prior))
  missing_current <- setdiff(fields, names(current))
  missing_cols <- union(missing_prior, missing_current)
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg fields} name column(s) absent from a register: {.val {missing_cols}}."
    )
  }

  prior_ids   <- as.character(prior$risk_id)
  current_ids <- as.character(current$risk_id)
  all_ids     <- union(prior_ids, current_ids)

  prior_pos   <- match(all_ids, prior_ids)
  current_pos <- match(all_ids, current_ids)
  in_prior    <- !is.na(prior_pos)
  in_current  <- !is.na(current_pos)

  # Per-field difference matrix over the common rows, used both to classify a
  # risk as changed/unchanged and to build the long field-change audit.
  changed_fields_list <- vector("list", length(all_ids))
  n_fields_changed <- integer(length(all_ids))

  for (i in seq_along(all_ids)) {
    if (in_prior[i] && in_current[i]) {
      diffs <- vapply(fields, function(f) {
        values_differ(prior[[f]][prior_pos[i]], current[[f]][current_pos[i]])
      }, logical(1))
      ch <- fields[diffs]
      changed_fields_list[[i]] <- ch
      n_fields_changed[i] <- length(ch)
    } else {
      changed_fields_list[[i]] <- character(0)
    }
  }

  change_type <- ifelse(
    !in_prior, "added",
    ifelse(!in_current, "removed",
           ifelse(n_fields_changed > 0L, "changed", "unchanged"))
  )

  rpn_prior   <- ifelse(in_prior,   prior$rpn[prior_pos],       NA_real_)
  rpn_current <- ifelse(in_current, current$rpn[current_pos],   NA_real_)
  rpn_delta   <- ifelse(in_prior & in_current, rpn_current - rpn_prior, NA_real_)
  level_prior   <- ifelse(in_prior,   prior$risk_level[prior_pos],     NA_character_)
  level_current <- ifelse(in_current, current$risk_level[current_pos], NA_character_)

  changed_fields <- vapply(changed_fields_list, function(ch) {
    if (length(ch) == 0L) NA_character_ else paste(ch, collapse = ", ")
  }, character(1))

  per_risk <- tibble::tibble(
    risk_id          = all_ids,
    change_type      = change_type,
    n_fields_changed = as.integer(n_fields_changed),
    changed_fields   = changed_fields,
    rpn_prior        = rpn_prior,
    rpn_current      = rpn_current,
    rpn_delta        = rpn_delta,
    level_prior      = level_prior,
    level_current    = level_current
  )

  # Order so the audit leads with what moved: changed, added, removed, then
  # unchanged; risk_id breaks ties for a deterministic order.
  type_rank <- match(per_risk$change_type,
                      c("changed", "added", "removed", "unchanged"))
  per_risk <- per_risk[order(type_rank, per_risk$risk_id), , drop = FALSE]

  # Long field-change audit for the changed risks only.
  fc_rows <- list()
  for (i in seq_along(all_ids)) {
    ch <- changed_fields_list[[i]]
    if (length(ch) == 0L) next
    fc_rows[[length(fc_rows) + 1L]] <- tibble::tibble(
      risk_id       = all_ids[i],
      field         = ch,
      value_prior   = unname(vapply(ch, function(f) fmt_value(prior[[f]][prior_pos[i]]),
                                    character(1))),
      value_current = unname(vapply(ch, function(f) fmt_value(current[[f]][current_pos[i]]),
                                    character(1)))
    )
  }
  field_changes <- if (length(fc_rows) > 0L) {
    out <- dplyr::bind_rows(fc_rows)
    out[order(out$risk_id, out$field), , drop = FALSE]
  } else {
    tibble::tibble(
      risk_id = character(0), field = character(0),
      value_prior = character(0), value_current = character(0)
    )
  }

  structure(
    list(
      per_risk          = per_risk,
      field_changes     = field_changes,
      n_prior           = length(prior_ids),
      n_current         = length(current_ids),
      n_added           = as.integer(sum(change_type == "added")),
      n_removed         = as.integer(sum(change_type == "removed")),
      n_changed         = as.integer(sum(change_type == "changed")),
      n_unchanged       = as.integer(sum(change_type == "unchanged")),
      total_rpn_prior   = if (nrow(prior) > 0L) sum(prior$rpn, na.rm = TRUE) else 0,
      total_rpn_current = if (nrow(current) > 0L) sum(current$rpn, na.rm = TRUE) else 0,
      total_rpn_delta   = (if (nrow(current) > 0L) sum(current$rpn, na.rm = TRUE) else 0) -
                          (if (nrow(prior) > 0L) sum(prior$rpn, na.rm = TRUE) else 0),
      fields            = fields
    ),
    class = "risk_reconciliation"
  )
}


#' Print a Risk Reconciliation
#' @param x A `risk_reconciliation` object.
#' @param ... Ignored.
#' @export
print.risk_reconciliation <- function(x, ...) {
  cli::cli_alert_info(
    "Risk Reconciliation: {x$n_prior} prior risk(s) -> {x$n_current} current"
  )
  cli::cli_alert_info(
    "  Added: {x$n_added}, removed: {x$n_removed}, \\
     changed: {x$n_changed}, unchanged: {x$n_unchanged}"
  )
  cli::cli_alert_info(
    "  Total RPN: {x$total_rpn_prior} -> {x$total_rpn_current} \\
     ({x$total_rpn_delta} delta)"
  )
  invisible(x)
}


#' Do two scalar values differ, treating NA == NA as equal?
#' @noRd
values_differ <- function(a, b) {
  a_na <- is.na(a)
  b_na <- is.na(b)
  if (a_na && b_na) return(FALSE)
  if (a_na || b_na) return(TRUE)
  !isTRUE(a == b)
}


#' Render a scalar value as a single display string for the change audit.
#' @noRd
fmt_value <- function(x) {
  if (is.na(x)) return(NA_character_)
  format(x, trim = TRUE)
}
