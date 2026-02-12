test_that("risk_indicator_summary returns expected indicators", {
  rr <- create_risk_register(make_test_risks())
  ind <- risk_indicator_summary(rr)

  expect_s3_class(ind, "tbl_df")
  expect_true(all(c("indicator", "value", "description") %in% names(ind)))
  expect_equal(nrow(ind), 7)

  expected_names <- c("RISK_TOTAL_COUNT", "RISK_OPEN_COUNT",
                       "RISK_CRITICAL_COUNT", "RISK_HIGH_COUNT",
                       "RISK_MEAN_RPN", "RISK_MAX_RPN",
                       "RISK_OVERALL_SCORE")
  expect_true(all(expected_names %in% ind$indicator))
})

test_that("risk_indicator_summary counts correctly", {
  rr <- create_risk_register(make_test_risks())
  ind <- risk_indicator_summary(rr)

  total <- ind$value[ind$indicator == "RISK_TOTAL_COUNT"]
  expect_equal(total, 4)

  open <- ind$value[ind$indicator == "RISK_OPEN_COUNT"]
  expect_equal(open, 4)  # all default to "open"
})

test_that("risk_indicator_summary handles empty register", {
  risks <- data.frame(
    risk_id = character(0), description = character(0),
    probability = numeric(0), impact = numeric(0),
    detectability = numeric(0)
  )
  rr <- create_risk_register(risks)
  ind <- risk_indicator_summary(rr)

  expect_equal(nrow(ind), 7)
  expect_true(all(ind$value == 0))
})

test_that("risk_indicator_summary overall_score bounded 0-1", {
  rr <- create_risk_register(make_test_risks())
  ind <- risk_indicator_summary(rr)

  overall <- ind$value[ind$indicator == "RISK_OVERALL_SCORE"]
  expect_true(overall >= 0 && overall <= 1)
})

test_that("risk_indicator_summary rejects non-risk_register", {
  expect_error(risk_indicator_summary(data.frame()), "must be a.*risk_register")
})
