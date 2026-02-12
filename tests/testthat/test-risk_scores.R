test_that("compute_risk_scores returns expected structure", {
  rr <- create_risk_register(make_test_risks())
  scores <- compute_risk_scores(rr)

  expect_s3_class(scores, "risk_scores")
  expect_true(is.numeric(scores$overall_risk_score))
  expect_true(is.numeric(scores$mean_rpn))
  expect_true(is.numeric(scores$max_rpn))
  expect_true(is.integer(scores$n_risks))
  expect_s3_class(scores$risk_distribution, "tbl_df")
  expect_s3_class(scores$category_summary, "tbl_df")
})

test_that("overall_risk_score is between 0 and 1", {
  rr <- create_risk_register(make_test_risks())
  scores <- compute_risk_scores(rr)
  expect_true(scores$overall_risk_score >= 0 && scores$overall_risk_score <= 1)
})

test_that("max RPN matches highest individual risk", {
  risks <- data.frame(
    risk_id = c("R001", "R002"),
    description = c("A", "B"),
    probability = c(5, 1), impact = c(5, 1), detectability = c(5, 1)
  )
  rr <- create_risk_register(risks)
  scores <- compute_risk_scores(rr)
  expect_equal(scores$max_rpn, 125)
})

test_that("empty risk register returns zero scores", {
  risks <- data.frame(
    risk_id = character(0), description = character(0),
    probability = numeric(0), impact = numeric(0),
    detectability = numeric(0)
  )
  rr <- create_risk_register(risks)
  scores <- compute_risk_scores(rr)

  expect_equal(scores$overall_risk_score, 0)
  expect_equal(scores$n_risks, 0L)
})

test_that("category_summary groups correctly", {
  rr <- create_risk_register(make_test_risks())
  scores <- compute_risk_scores(rr)

  # make_test_risks has 2 data_quality + 1 traceability + 1 documentation
  dq <- scores$category_summary[scores$category_summary$category == "data_quality", ]
  expect_equal(dq$n, 2L)
})

test_that("print.risk_scores does not error", {
  rr <- create_risk_register(make_test_risks())
  scores <- compute_risk_scores(rr)
  expect_no_error(print(scores))
})
