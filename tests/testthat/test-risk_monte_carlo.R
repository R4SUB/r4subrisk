# Tests for risk_monte_carlo() (R/risk_monte_carlo.R)

mk_reg <- function() {
  risks <- data.frame(
    risk_id = c("R001", "R002", "R003"),
    description = c("Missing SDTM vars", "Unmapped derivations", "Define drift"),
    probability = c(5, 3, 2), impact = c(5, 4, 3), detectability = c(4, 3, 2),
    stringsAsFactors = FALSE
  )
  suppressMessages(create_risk_register(risks))
}

test_that("spread = 0 reproduces the deterministic RPN", {
  rr <- mk_reg()
  mc <- risk_monte_carlo(rr, n = 500, spread = 0)
  expect_s3_class(mc, "risk_monte_carlo")
  # point RPNs: 100, 36, 12 -> total 148
  expect_equal(mc$point_total, 148)
  expect_equal(unname(mc$total[["mean"]]), 148)
  expect_equal(unname(mc$total[["sd"]]), 0)
  expect_equal(mc$per_risk$mc_mean[mc$per_risk$risk_id == "R001"], 100)
})

test_that("seed makes the simulation reproducible", {
  rr <- mk_reg()
  a <- risk_monte_carlo(rr, n = 1000, seed = 42)
  b <- risk_monte_carlo(rr, n = 1000, seed = 42)
  expect_identical(a$total_draws, b$total_draws)
  expect_equal(a$per_risk, b$per_risk)
})

test_that("the caller's RNG stream is not disturbed", {
  rr <- mk_reg()
  set.seed(7)
  before <- runif(1)
  set.seed(7)
  invisible(risk_monte_carlo(rr, n = 200, seed = 99))
  after <- runif(1)
  expect_equal(before, after)
})

test_that("draws stay on the 1-125 RPN scale", {
  rr <- mk_reg()
  mc <- risk_monte_carlo(rr, n = 2000, spread = 2, seed = 1)
  expect_true(all(mc$total_draws >= 3))     # 3 risks * min RPN 1
  expect_true(all(mc$total_draws <= 375))   # 3 risks * max RPN 125
  expect_true(all(mc$per_risk$p95 <= 125))
  expect_true(all(mc$per_risk$p05 >= 1))
})

test_that("a high-scoring risk has a high probability of being critical", {
  rr <- mk_reg()
  mc <- risk_monte_carlo(rr, n = 5000, spread = 1, seed = 3)
  # R001 (5,5,4 = 100) should almost always be critical (>= 80)
  pc <- mc$per_risk$prob_critical[mc$per_risk$risk_id == "R001"]
  expect_gt(pc, 0.5)
  # R003 (2,3,2 = 12) should essentially never be critical
  pc3 <- mc$per_risk$prob_critical[mc$per_risk$risk_id == "R003"]
  expect_lt(pc3, 0.05)
})

test_that("per_risk is ordered by probability of being critical", {
  rr <- mk_reg()
  mc <- risk_monte_carlo(rr, n = 2000, seed = 5)
  expect_equal(mc$per_risk$risk_id[1], "R001")
  expect_true(!is.unsorted(rev(mc$per_risk$prob_critical)))
})

test_that("total_draws has the requested length and quantiles are ordered", {
  rr <- mk_reg()
  mc <- risk_monte_carlo(rr, n = 1234, seed = 2)
  expect_length(mc$total_draws, 1234L)
  expect_lte(mc$total[["p05"]], mc$total[["p50"]])
  expect_lte(mc$total[["p50"]], mc$total[["p95"]])
})

test_that("plain data.frame with the FMEA columns is accepted", {
  df <- data.frame(
    risk_id = c("A", "B"), probability = c(3, 4),
    impact = c(3, 4), detectability = c(3, 4)
  )
  mc <- risk_monte_carlo(df, n = 300, spread = 0)
  expect_equal(mc$point_total, 27 + 64)
})

test_that("bad input is rejected", {
  rr <- mk_reg()
  expect_error(risk_monte_carlo(data.frame(risk_id = "A")), "missing")
  expect_error(risk_monte_carlo(rr[0, ]), "no risks")
  expect_error(risk_monte_carlo(rr, n = 0), "positive")
  expect_error(risk_monte_carlo(rr, spread = -1), "non-negative")
})
