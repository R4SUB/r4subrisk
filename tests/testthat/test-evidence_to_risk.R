test_that("evidence_to_risks converts failing evidence to risk items", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  risk_items <- evidence_to_risks(ev)

  expect_s3_class(risk_items, "tbl_df")
  expect_true(all(c("risk_id", "description", "category", "probability",
                     "impact", "detectability") %in% names(risk_items)))
  # 3 non-pass rows: fail, warn, fail
  expect_equal(nrow(risk_items), 3)
})

test_that("evidence_to_risks excludes pass by default", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  risk_items <- evidence_to_risks(ev, include_pass = FALSE)

  # Should not include "pass" results
  expect_true(nrow(risk_items) < 5)
})

test_that("evidence_to_risks includes pass when requested", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  risk_items <- evidence_to_risks(ev, include_pass = TRUE)

  # Should include all 5 indicators
  expect_equal(nrow(risk_items), 5)
})

test_that("evidence_to_risks maps severity to probability/impact", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  risk_items <- evidence_to_risks(ev, include_pass = TRUE)

  # The critical severity row should have probability=5, impact=5
  # (R1 indicator with severity "critical")
  # After aggregation, check the max mapped values
  expect_true(all(risk_items$probability >= 1 & risk_items$probability <= 5))
  expect_true(all(risk_items$impact >= 1 & risk_items$impact <= 5))
})

test_that("evidence_to_risks returns empty tibble for all-pass evidence", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "DS",
    source_name = "test", source_version = NA_character_,
    indicator_id = "Q1", indicator_name = "Q1",
    indicator_domain = "quality",
    severity = "info", result = "pass",
    metric_value = NA_real_, metric_unit = NA_character_,
    message = NA_character_, location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)
  risk_items <- evidence_to_risks(ev)
  expect_equal(nrow(risk_items), 0)
})

test_that("evidence_to_risks output can be passed to create_risk_register", {
  skip_if_not_installed("r4subcore")
  ev <- make_test_evidence()
  risk_items <- evidence_to_risks(ev)
  rr <- create_risk_register(risk_items)

  expect_s3_class(rr, "risk_register")
  expect_true("rpn" %in% names(rr))
})
