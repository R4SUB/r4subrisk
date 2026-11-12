# Tests for risk_pareto / plot_risk_pareto (R/risk_pareto.R)

test_that("risk_pareto ranks by descending RPN with the expected columns", {
  rr <- create_risk_register(make_test_risks())
  pr <- risk_pareto(rr)

  expect_s3_class(pr, "risk_pareto")
  expect_s3_class(pr, "tbl_df")
  expect_setequal(
    names(pr),
    c("rank", "risk_id", "description", "category", "rpn", "risk_level",
      "rpn_share", "cum_rpn", "cum_share", "vital_few")
  )
  expect_equal(pr$rank, seq_len(nrow(pr)))
  # RPN is non-increasing down the ranking
  expect_true(all(diff(pr$rpn) <= 0))
  # highest RPN is R001 (4*5*2 = 40)
  expect_equal(pr$risk_id[1], "R001")
  expect_equal(pr$rpn[1], 40)
})

test_that("shares and cumulative shares are consistent and reuse stored RPN", {
  rr <- create_risk_register(make_test_risks())
  pr <- risk_pareto(rr)

  total <- sum(rr$rpn)
  expect_equal(attr(pr, "total_rpn"), total)
  expect_equal(pr$rpn_share, round(pr$rpn / total, 4))
  expect_equal(pr$cum_rpn, cumsum(pr$rpn))
  expect_equal(pr$cum_share[nrow(pr)], 1)
  # cumulative share is monotonically non-decreasing
  expect_true(all(diff(pr$cum_share) >= 0))
})

test_that("vital few is the shortest top run reaching the cutoff", {
  rr <- create_risk_register(make_test_risks())
  pr <- risk_pareto(rr, cum_cutoff = 0.8)

  k <- which(pr$cum_share >= 0.8)[1]
  expect_equal(pr$vital_few, seq_len(nrow(pr)) <= k)
  expect_equal(attr(pr, "n_vital_few"), as.integer(k))
  expect_equal(attr(pr, "cum_cutoff"), 0.8)
  # the vital few alone must clear the cutoff
  expect_gte(sum(pr$rpn[pr$vital_few]) / sum(pr$rpn), 0.8)
})

test_that("a larger cutoff never flags fewer risks", {
  rr <- create_risk_register(make_test_risks())
  n_small <- attr(risk_pareto(rr, cum_cutoff = 0.5), "n_vital_few")
  n_large <- attr(risk_pareto(rr, cum_cutoff = 0.95), "n_vital_few")
  expect_gte(n_large, n_small)

  # cutoff of 1 flags every risk
  expect_equal(attr(risk_pareto(rr, cum_cutoff = 1), "n_vital_few"), nrow(rr))
})

test_that("ties are broken deterministically by risk_id", {
  rr <- create_risk_register(data.frame(
    risk_id       = c("R002", "R001"),
    description   = c("b", "a"),
    probability   = c(3, 3), impact = c(3, 3), detectability = c(3, 3),
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr)
  expect_equal(pr$rpn, c(27, 27))
  expect_equal(pr$risk_id, c("R001", "R002"))
})

test_that("empty register yields an empty, well-formed table", {
  rr <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr)
  expect_s3_class(pr, "risk_pareto")
  expect_equal(nrow(pr), 0L)
  expect_equal(attr(pr, "total_rpn"), 0)
  expect_equal(attr(pr, "n_vital_few"), 0L)
})

test_that("invalid input is rejected", {
  rr <- create_risk_register(make_test_risks())
  expect_error(risk_pareto(make_test_risks()), "risk_register")
  expect_error(risk_pareto(rr, cum_cutoff = 0), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = 1.5), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = c(0.5, 0.8)), "cum_cutoff")
})

test_that("print returns its input invisibly", {
  rr <- create_risk_register(make_test_risks())
  pr <- risk_pareto(rr)
  expect_message(print(pr), "Risk Pareto")
  expect_output(print(pr), "rank")
  expect_identical(withVisible(print(pr))$visible, FALSE)
})

test_that("plot_risk_pareto returns a ggplot and accepts a precomputed table", {
  skip_if_not_installed("ggplot2")
  rr <- create_risk_register(make_test_risks())

  p <- plot_risk_pareto(rr)
  expect_s3_class(p, "ggplot")

  p2 <- plot_risk_pareto(risk_pareto(rr, cum_cutoff = 0.9))
  expect_s3_class(p2, "ggplot")
})

test_that("plot_risk_pareto errors on an empty register", {
  skip_if_not_installed("ggplot2")
  rr <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  expect_error(plot_risk_pareto(rr), "empty")
})
