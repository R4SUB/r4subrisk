test_that("create_risk_register produces correct structure", {
  rr <- create_risk_register(make_test_risks())
  expect_s3_class(rr, "risk_register")
  expect_true(all(c("risk_id", "description", "category", "probability",
                     "impact", "detectability", "rpn", "risk_level")
                   %in% names(rr)))
  expect_equal(nrow(rr), 4)
})

test_that("create_risk_register computes RPN correctly", {
  risks <- data.frame(
    risk_id = "R001", description = "Test risk",
    probability = 4, impact = 5, detectability = 2
  )
  rr <- create_risk_register(risks)
  expect_equal(rr$rpn[1], 4 * 5 * 2)  # 40
})

test_that("create_risk_register classifies risk levels", {
  risks <- data.frame(
    risk_id     = c("R001", "R002", "R003"),
    description = c("Critical", "Medium", "Low"),
    probability   = c(5, 3, 1),
    impact        = c(5, 3, 2),
    detectability = c(5, 3, 1)
  )
  rr <- create_risk_register(risks)
  expect_equal(rr$risk_level[1], "critical")  # RPN=125
  expect_equal(rr$risk_level[2], "medium")    # RPN=27
  expect_equal(rr$risk_level[3], "low")       # RPN=2
})

test_that("create_risk_register fills defaults for optional columns", {
  risks <- data.frame(
    risk_id = "R001", description = "Test risk"
  )
  rr <- create_risk_register(risks)
  expect_equal(rr$probability[1], 3)
  expect_equal(rr$impact[1], 3)
  expect_equal(rr$category[1], "general")
  expect_equal(rr$status[1], "open")
})

test_that("create_risk_register errors on missing required columns", {
  expect_error(
    create_risk_register(data.frame(description = "test")),
    "missing required column"
  )
})

test_that("create_risk_register rejects non-data.frame", {
  expect_error(
    create_risk_register("not_a_df"),
    "must be a data.frame"
  )
})

test_that("create_risk_register rejects duplicate risk_ids", {
  risks <- data.frame(
    risk_id = c("R001", "R001"),
    description = c("A", "B"),
    probability = c(3, 3), impact = c(3, 3), detectability = c(3, 3)
  )
  expect_error(create_risk_register(risks), "Duplicate risk_id")
})

test_that("create_risk_register validates FMEA scales", {
  risks <- data.frame(
    risk_id = "R001", description = "Test",
    probability = 6, impact = 3, detectability = 3
  )
  expect_error(create_risk_register(risks), "between 1 and 5")
})

test_that("print.risk_register does not error", {
  rr <- create_risk_register(make_test_risks())
  expect_no_error(print(rr))
})
