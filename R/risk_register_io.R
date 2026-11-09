#' Write a Risk Register to Disk
#'
#' Exports a risk register to a flat file so it can be shared, reviewed outside
#' R, or re-imported later with [read_risk_register()]. All columns are written,
#' including the computed `rpn` and `risk_level`, so the file is a faithful,
#' human-readable snapshot. Those computed columns are *not* trusted on
#' re-import: [read_risk_register()] rebuilds the register through
#' [create_risk_register()], recomputing the FMEA scores from the assessed
#' probability, impact, and detectability. A write -> read cycle therefore
#' reproduces an equivalent register.
#'
#' @param register A `risk_register` from [create_risk_register()].
#' @param path Output file path. A `.xlsx` extension writes an Excel workbook
#'   (requires the \pkg{writexl} package); any other extension writes CSV.
#'
#' @return `path`, invisibly.
#'
#' @seealso [read_risk_register()] for the inverse, [create_risk_register()].
#'
#' @examples
#' rr <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002"),
#'   description   = c("Missing vars", "Bad derivation"),
#'   probability   = c(4, 2),
#'   impact        = c(5, 3),
#'   detectability = c(2, 3)
#' ))
#' path <- file.path(tempdir(), "risk_register.csv")
#' write_risk_register(rr, path)
#' read_risk_register(path)
#' unlink(path)
#'
#' @export
write_risk_register <- function(register, path) {
  if (!inherits(register, "risk_register")) {
    cli::cli_abort("{.arg register} must be a {.cls risk_register} object.")
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty file path.")
  }

  df <- as.data.frame(register, stringsAsFactors = FALSE)

  if (is_xlsx_path(path)) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg writexl} is required to write {.file .xlsx} files.",
        "i" = "Install it with {.code install.packages(\"writexl\")}, \\
               or write a {.file .csv} file instead."
      ))
    }
    writexl::write_xlsx(df, path = path)
  } else {
    utils::write.csv(df, file = path, row.names = FALSE, na = "NA")
  }

  invisible(path)
}


#' Read a Risk Register from Disk
#'
#' Imports a risk register previously written by [write_risk_register()] (or any
#' compatible CSV / xlsx carrying at least `risk_id` and `description`) and
#' rebuilds it through [create_risk_register()]. The canonical schema, RPN, and
#' risk-level classification are therefore reconstructed from the assessed
#' scores rather than trusted from the file, so an imported register is always
#' internally consistent.
#'
#' @param path Input file path. A `.xlsx` extension reads an Excel workbook
#'   (requires the \pkg{readxl} package); any other extension reads CSV.
#' @param config A `risk_config` from [risk_config_default()], forwarded to
#'   [create_risk_register()] for RPN classification.
#'
#' @return A tibble of class `"risk_register"`.
#'
#' @seealso [write_risk_register()] for the inverse, [create_risk_register()].
#'
#' @examples
#' rr <- create_risk_register(data.frame(
#'   risk_id       = c("R001", "R002"),
#'   description   = c("Missing vars", "Bad derivation"),
#'   probability   = c(4, 2),
#'   impact        = c(5, 3),
#'   detectability = c(2, 3)
#' ))
#' path <- file.path(tempdir(), "risk_register.csv")
#' write_risk_register(rr, path)
#' rr2 <- read_risk_register(path)
#' identical(rr$rpn, rr2$rpn)
#' unlink(path)
#'
#' @export
read_risk_register <- function(path, config = risk_config_default()) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty file path.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}.")
  }

  if (is_xlsx_path(path)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg readxl} is required to read {.file .xlsx} files.",
        "i" = "Install it with {.code install.packages(\"readxl\")}, \\
               or read a {.file .csv} file instead."
      ))
    }
    raw <- as.data.frame(readxl::read_xlsx(path), stringsAsFactors = FALSE)
  } else {
    raw <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "NA")
  }

  # Drop any stored computed columns: create_risk_register() recomputes RPN and
  # the risk level from the assessed scores, so they are never trusted from the
  # file.
  raw <- raw[, setdiff(names(raw), c("rpn", "risk_level")), drop = FALSE]

  create_risk_register(raw, config = config)
}


#' Is this an xlsx path?
#' @noRd
is_xlsx_path <- function(path) {
  grepl("\\.xlsx$", path, ignore.case = TRUE)
}
