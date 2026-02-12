test_that("risk_config_default returns expected structure", {
  cfg <- risk_config_default()
  expect_s3_class(cfg, "risk_config")
  expect_named(cfg, c("rpn_bands", "evidence_severity_to_probability",
                       "evidence_severity_to_impact", "default_detectability"))
  expect_length(cfg$rpn_bands, 4)
  expect_equal(cfg$default_detectability, 3)
})

test_that("risk_config_default validates detectability range", {
  expect_error(risk_config_default(default_detectability = 0), "between 1 and 5")
  expect_error(risk_config_default(default_detectability = 6), "between 1 and 5")
})

test_that("risk_config_default accepts custom bands", {
  cfg <- risk_config_default(
    rpn_bands = list(bad = c(50, 125), ok = c(1, 49))
  )
  expect_equal(names(cfg$rpn_bands), c("bad", "ok"))
})

test_that("classify_rpn returns correct levels", {
  expect_equal(classify_rpn(100), "critical")
  expect_equal(classify_rpn(80), "critical")
  expect_equal(classify_rpn(50), "high")
  expect_equal(classify_rpn(25), "medium")
  expect_equal(classify_rpn(5), "low")
  expect_equal(classify_rpn(1), "low")
})

test_that("classify_rpn rejects non-numeric", {
  expect_error(classify_rpn("high"), "single numeric")
})

test_that("classify_rpn works with custom bands", {
  bands <- list(red = c(50, 125), yellow = c(20, 49), green = c(1, 19))
  expect_equal(classify_rpn(60, bands), "red")
  expect_equal(classify_rpn(30, bands), "yellow")
  expect_equal(classify_rpn(10, bands), "green")
})
