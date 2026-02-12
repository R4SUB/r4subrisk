# Shared test helper: create a small risk register
make_test_risks <- function() {
  data.frame(
    risk_id       = c("R001", "R002", "R003", "R004"),
    description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                       "Inconsistent define.xml", "Label mismatches"),
    category      = c("data_quality", "traceability", "documentation", "data_quality"),
    probability   = c(4, 3, 2, 1),
    impact        = c(5, 4, 3, 2),
    detectability = c(2, 3, 4, 1),
    stringsAsFactors = FALSE
  )
}

# Shared helper: create test evidence
make_test_evidence <- function() {
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")

  ev <- data.frame(
    asset_type       = "dataset",
    asset_id         = rep("ADSL", 5),
    source_name      = "test_source",
    source_version   = NA_character_,
    indicator_id     = c("Q1", "Q2", "T1", "R1", "U1"),
    indicator_name   = c("Quality Check 1", "Quality Check 2",
                         "Trace Check 1", "Risk Check 1",
                         "Usability Check 1"),
    indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
    severity         = c("high", "low", "medium", "critical", "info"),
    result           = c("fail", "pass", "warn", "fail", "pass"),
    metric_value     = NA_real_,
    metric_unit      = NA_character_,
    message          = c("Variable missing", "OK", "Partial trace",
                         "Critical gap", "Readable"),
    location         = NA_character_,
    evidence_payload = "{}",
    stringsAsFactors = FALSE
  )

  r4subcore::as_evidence(ev, ctx = ctx)
}
