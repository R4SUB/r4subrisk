# Tests for risk_reduction_summary (R/risk_reduction.R)

test_that("reduction summary reports total and per-risk reduction", {
  rr <- create_risk_register(make_test_risks())
  rr2 <- apply_mitigations(
    rr, data.frame(risk_id = c("R001", "R002"), probability = c(1, 1))
  )

  s <- risk_reduction_summary(rr, rr2)

  expect_s3_class(s, "risk_reduction")
  expect_equal(s$total_rpn_before, sum(rr$rpn))
  expect_equal(s$total_rpn_after, sum(rr2$rpn))
  expect_gt(s$total_rpn_reduction, 0)
  expect_gt(s$pct_reduction, 0)
  expect_equal(nrow(s$per_risk), 4)
})

test_that("mitigation that lowers a level is counted as a downgrade", {
  rr <- create_risk_register(make_test_risks())
  # R001 starts critical (4*5*2 = 40 -> high actually). Force a clear drop.
  rr2 <- apply_mitigations(
    rr, data.frame(risk_id = "R001", probability = 1, impact = 1,
                   detectability = 1)
  )
  s <- risk_reduction_summary(rr, rr2)

  expect_gte(s$n_downgraded, 1)
  expect_gte(s$n_reduced, 1)
})

test_that("no change yields zero reduction", {
  rr <- create_risk_register(make_test_risks())
  s <- risk_reduction_summary(rr, rr)

  expect_equal(s$total_rpn_reduction, 0)
  expect_equal(s$pct_reduction, 0)
  expect_equal(s$n_reduced, 0L)
  expect_equal(s$n_downgraded, 0L)
})

test_that("added and resolved risks are counted", {
  r1 <- create_risk_register(make_test_risks())
  r2_df <- make_test_risks()[1:2, ]
  r2_df <- rbind(r2_df, data.frame(
    risk_id = "R099", description = "new", category = "general",
    probability = 2, impact = 2, detectability = 2
  ))
  r2 <- create_risk_register(r2_df)

  s <- risk_reduction_summary(r1, r2)
  expect_equal(s$n_added, 1L)
  expect_equal(s$n_resolved, 2L)
  expect_equal(s$n_common, 2L)
})

test_that("print returns invisibly", {
  rr <- create_risk_register(make_test_risks())
  s <- risk_reduction_summary(rr, rr)
  expect_invisible(print(s))
})
