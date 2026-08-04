# Tests for pluggable risk models (R/risk_models.R)

test_that("the FMEA model reproduces the register RPN", {
  risks <- make_test_risks()
  rr <- create_risk_register(risks)
  scored <- apply_risk_model(risks, risk_model_fmea())

  expect_equal(scored$raw_score, rr$rpn)
  expect_equal(scored$risk_level, rr$risk_level)
  expect_true(all(scored$risk_score >= 0 & scored$risk_score <= 100))
})

test_that("the two-factor model ignores detectability", {
  risks <- make_test_risks()
  scored <- apply_risk_model(risks, risk_model_probability_impact())

  expect_equal(scored$raw_score, risks$probability * risks$impact)
  expect_equal(scored$model[1], "probability_impact")
})

test_that("the ICH Q9 model produces qualitative levels", {
  risks <- make_test_risks()
  scored <- apply_risk_model(risks, risk_model_ich_q9())

  expect_true(all(scored$risk_level %in%
                    c("acceptable", "tolerable", "unacceptable")))
  expect_true(all(scored$risk_score >= 0 & scored$risk_score <= 100))
})

test_that("every model returns a 0-100 normalized score", {
  risks <- make_test_risks()
  for (m in list(risk_model_fmea(), risk_model_probability_impact(),
                 risk_model_ich_q9())) {
    scored <- apply_risk_model(risks, m)
    expect_true(all(scored$risk_score >= 0 & scored$risk_score <= 100))
    overall <- risk_model_overall(scored)
    expect_true(overall >= 0 && overall <= 100)
  }
})

test_that("custom models work through the same interface", {
  m <- risk_model(
    name = "impact_only",
    score = function(df) df$impact,
    max_raw = 5,
    bands = list(high = c(4, 5), medium = c(3, 3), low = c(1, 2)),
    required = "impact"
  )
  scored <- apply_risk_model(make_test_risks(), m)
  expect_equal(scored$raw_score, make_test_risks()$impact)
  expect_equal(scored$risk_score, round(make_test_risks()$impact / 5 * 100, 1))
})

test_that("missing required columns are reported", {
  bad <- data.frame(risk_id = "R1", probability = 3)
  expect_error(
    apply_risk_model(bad, risk_model_probability_impact()),
    "impact"
  )
})

test_that("model constructor validates its inputs", {
  expect_error(risk_model("m", "notfun", 5, list(a = c(1, 2))), "function")
  expect_error(
    risk_model("m", function(df) df$x, -1, list(a = c(1, 2))),
    "positive"
  )
})
