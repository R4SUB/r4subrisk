# Tests for the risk heatmap (R/risk_heatmap.R)

test_that("heatmap data covers the full 5 by 5 grid", {
  rr <- create_risk_register(make_test_risks())
  dat <- risk_heatmap_data(rr)

  expect_equal(nrow(dat), 25L)
  expect_setequal(unique(dat$probability), 1:5)
  expect_setequal(unique(dat$impact), 1:5)
})

test_that("occupied cells carry the right counts and RPN", {
  rr <- create_risk_register(make_test_risks())
  dat <- risk_heatmap_data(rr)

  # R001 is probability 4, impact 5, rpn 40.
  cell <- dat[dat$probability == 4 & dat$impact == 5, ]
  expect_equal(cell$n, 1L)
  expect_equal(cell$mean_rpn, 40)
  expect_false(is.na(cell$zone))

  # Total counts across the grid equal the number of risks.
  expect_equal(sum(dat$n), nrow(rr))
})

test_that("empty cells have NA metrics", {
  rr <- create_risk_register(make_test_risks())
  dat <- risk_heatmap_data(rr)
  empty <- dat[dat$n == 0L, ]
  expect_true(all(is.na(empty$mean_rpn)))
  expect_true(all(is.na(empty$zone)))
})

test_that("plot_risk_heatmap returns a ggplot", {
  skip_if_not_installed("ggplot2")
  rr <- create_risk_register(make_test_risks())
  p <- plot_risk_heatmap(rr)
  expect_s3_class(p, "ggplot")
})

test_that("plot_risk_heatmap accepts each metric", {
  skip_if_not_installed("ggplot2")
  rr <- create_risk_register(make_test_risks())
  for (m in c("mean_rpn", "max_rpn", "n")) {
    expect_s3_class(plot_risk_heatmap(rr, metric = m), "ggplot")
  }
})
