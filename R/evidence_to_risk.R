#' Derive Risk Items from Evidence
#'
#' Automatically generates risk items from an R4SUB evidence table.
#' Each failing or warning indicator becomes a potential risk, with
#' probability and impact inferred from evidence severity.
#'
#' @param evidence A validated evidence data.frame (from `r4subcore`).
#' @param config A `risk_config` from [risk_config_default()].
#' @param include_pass Logical; if `TRUE`, passing indicators are also
#'   included as low-risk items. Default `FALSE`.
#'
#' @return A tibble suitable for [create_risk_register()].
#'
#' @details
#' The mapping from evidence to risk uses:
#' - `risk_id`: derived from `indicator_id` + `asset_id` via
#'   `r4subcore::hash_id()`
#' - `category`: mapped from `indicator_domain`
#' - `probability`: mapped from evidence `severity` via config
#' - `impact`: mapped from evidence `severity` via config
#' - `detectability`: uses `config$default_detectability`
#'
#' Multiple evidence rows for the same indicator + asset are aggregated:
#' probability and impact use the maximum across rows.
#'
#' @examples
#' \dontrun{
#' risk_items <- evidence_to_risks(evidence)
#' rr <- create_risk_register(risk_items)
#' }
#'
#' @export
evidence_to_risks <- function(evidence,
                              config = risk_config_default(),
                              include_pass = FALSE) {
  r4subcore::validate_evidence(evidence)

  ev <- evidence

  # Filter to non-pass if requested

  if (!include_pass) {
    ev <- ev[ev$result != "pass", , drop = FALSE]
  }

  if (nrow(ev) == 0L) {
    return(tibble::tibble(
      risk_id        = character(0),
      description    = character(0),
      category       = character(0),
      probability    = numeric(0),
      impact         = numeric(0),
      detectability  = numeric(0),
      status         = character(0)
    ))
  }

  prob_map <- config$evidence_severity_to_probability
  imp_map  <- config$evidence_severity_to_impact

  # Map severity to probability/impact
  ev$prob <- unname(prob_map[ev$severity])
  ev$imp  <- unname(imp_map[ev$severity])

  # Fill NA mappings with midpoint
  ev$prob[is.na(ev$prob)] <- 3
  ev$imp[is.na(ev$imp)]   <- 3

  # Aggregate by indicator_id + asset_id
  grp_key <- paste0(ev$indicator_id, "\x01", ev$asset_id)
  groups <- split(seq_len(nrow(ev)), grp_key)

  rows <- lapply(names(groups), function(g) {
    idx <- groups[[g]]
    first <- idx[1]

    risk_id <- r4subcore::hash_id(
      ev$indicator_id[first], ev$asset_id[first],
      prefix = "RISK"
    )

    # Description from indicator name + message
    desc_parts <- unique(c(ev$indicator_name[idx]))
    msgs <- ev$message[idx]
    msgs <- msgs[!is.na(msgs)]
    if (length(msgs) > 0L) {
      desc_parts <- c(desc_parts, paste0("[", msgs[1], "]"))
    }

    tibble::tibble(
      risk_id       = risk_id,
      description   = paste(desc_parts, collapse = " - "),
      category      = ev$indicator_domain[first],
      probability   = max(ev$prob[idx], na.rm = TRUE),
      impact        = max(ev$imp[idx], na.rm = TRUE),
      detectability = config$default_detectability,
      status        = "open"
    )
  })

  dplyr::bind_rows(rows)
}
