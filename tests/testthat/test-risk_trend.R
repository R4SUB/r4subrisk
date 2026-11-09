# Tests for risk_trend / plot_risk_trend (R/risk_trend.R)

make_trend_registers <- function() {
  m1 <- create_risk_register(data.frame(
    risk_id       = c("R001", "R002", "R003"),
    description   = c("Missing vars", "Bad derivation", "Define gap"),
    probability   = c(4, 5, 2),
    impact        = c(5, 5, 3),
    detectability = c(4, 4, 4),
    stringsAsFactors = FALSE
  ))
  m2 <- apply_mitigations(
    m1, data.frame(risk_id = c("R001", "R002"), probability = c(2, 2))
  )
  list(baseline = m1, interim = m2)
}

test_that("risk_trend returns one tidy row per milestone", {
  regs <- make_trend_registers()
  tr <- risk_trend(regs)

  expect_s3_class(tr, "risk_trend")
  expect_s3_class(tr, "tbl_df")
  expect_equal(nrow(tr), 2L)
  expect_equal(tr$milestone, c("baseline", "interim"))
  expect_equal(tr$milestone_index, c(1L, 2L))
  expect_setequal(
    names(tr),
    c("milestone", "milestone_index", "n_risks", "total_rpn",
      "mean_rpn", "n_open_critical")
  )
})

test_that("totals and means reuse the registers' stored RPN", {
  regs <- make_trend_registers()
  tr <- risk_trend(regs)

  expect_equal(tr$total_rpn[1], sum(regs$baseline$rpn))
  expect_equal(tr$total_rpn[2], sum(regs$interim$rpn))
  expect_equal(tr$mean_rpn[1], round(mean(regs$baseline$rpn), 1))
  expect_equal(tr$n_risks, c(3L, 3L))
})

test_that("open-critical count falls as risk is mitigated (burndown)", {
  regs <- make_trend_registers()
  tr <- risk_trend(regs)

  # baseline: R001 (80) and R002 (100) are critical and open
  expect_equal(tr$n_open_critical[1], 2L)
  # after mitigation both drop out of the critical band
  expect_equal(tr$n_open_critical[2], 0L)
  expect_true(tr$total_rpn[2] < tr$total_rpn[1])
})

test_that("a resolved critical risk is not counted as open-critical", {
  m1 <- create_risk_register(data.frame(
    risk_id = "R001", description = "Critical issue",
    probability = 5, impact = 5, detectability = 4, status = "open",
    stringsAsFactors = FALSE
  ))
  m2 <- create_risk_register(data.frame(
    risk_id = "R001", description = "Critical issue",
    probability = 5, impact = 5, detectability = 4, status = "closed",
    stringsAsFactors = FALSE
  ))
  tr <- risk_trend(list(before = m1, after = m2))

  expect_equal(tr$n_open_critical, c(1L, 0L))
})

test_that("unnamed and blank milestones get fallback labels", {
  regs <- unname(make_trend_registers())
  tr <- risk_trend(regs)
  expect_equal(tr$milestone, c("milestone_1", "milestone_2"))
})

test_that("empty register contributes zeroes", {
  empty <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  tr <- risk_trend(list(m1 = empty))
  expect_equal(tr$total_rpn, 0)
  expect_equal(tr$mean_rpn, 0)
  expect_equal(tr$n_risks, 0L)
  expect_equal(tr$n_open_critical, 0L)
})

test_that("invalid input is rejected", {
  expect_error(risk_trend(list()), "at least one")
  expect_error(risk_trend(list(1, 2)), "risk_register")
  expect_error(risk_trend(create_risk_register(make_test_risks())),
               "list of")
})

test_that("plot_risk_trend returns a ggplot", {
  skip_if_not_installed("ggplot2")
  regs <- make_trend_registers()

  p <- plot_risk_trend(regs)
  expect_s3_class(p, "ggplot")

  # also accepts a precomputed trend tibble and other metrics
  p2 <- plot_risk_trend(risk_trend(regs), metric = "n_open_critical")
  expect_s3_class(p2, "ggplot")
})
