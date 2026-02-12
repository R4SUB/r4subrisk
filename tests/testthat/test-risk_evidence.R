test_that("risk_register_to_evidence returns valid evidence", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")
  rr <- create_risk_register(make_test_risks())
  ev <- risk_register_to_evidence(rr, ctx = ctx)

  expect_true(r4subcore::validate_evidence(ev))
  # 4 risk items + 1 aggregate = 5 rows
  expect_equal(nrow(ev), 5)
})

test_that("risk_register_to_evidence emits RISK_ITEM rows", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")
  rr <- create_risk_register(make_test_risks())
  ev <- risk_register_to_evidence(rr, ctx = ctx)

  item_rows <- ev[ev$indicator_id == "RISK_ITEM", ]
  expect_equal(nrow(item_rows), 4)
  expect_true(all(item_rows$indicator_domain == "risk"))
  expect_true(all(item_rows$metric_unit == "RPN"))
})

test_that("risk_register_to_evidence emits RISK_OVERALL_SCORE row", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")
  rr <- create_risk_register(make_test_risks())
  ev <- risk_register_to_evidence(rr, ctx = ctx)

  agg_rows <- ev[ev$indicator_id == "RISK_OVERALL_SCORE", ]
  expect_equal(nrow(agg_rows), 1)
  expect_true(agg_rows$metric_value[1] >= 0 && agg_rows$metric_value[1] <= 1)
})

test_that("risk_register_to_evidence maps severity from risk_level", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")

  risks <- data.frame(
    risk_id = c("R001", "R002"),
    description = c("Critical risk", "Low risk"),
    probability = c(5, 1), impact = c(5, 2), detectability = c(5, 1)
  )
  rr <- create_risk_register(risks)
  ev <- risk_register_to_evidence(rr, ctx = ctx)

  item_rows <- ev[ev$indicator_id == "RISK_ITEM", ]
  crit_row <- item_rows[item_rows$asset_id == "R001", ]
  low_row  <- item_rows[item_rows$asset_id == "R002", ]

  expect_equal(crit_row$severity[1], "critical")
  expect_equal(crit_row$result[1], "fail")
  expect_equal(low_row$severity[1], "low")
  expect_equal(low_row$result[1], "pass")
})

test_that("risk_register_to_evidence sets source_name and source_version", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")
  rr <- create_risk_register(make_test_risks())
  ev <- risk_register_to_evidence(rr, ctx = ctx,
                                   source_name = "my_tool",
                                   source_version = "2.0.0")

  expect_true(all(ev$source_name == "my_tool"))
  expect_true(all(ev$source_version == "2.0.0"))
})

test_that("risk_register_to_evidence includes RPN in metric_value", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "TEST001", environment = "DEV")

  risks <- data.frame(
    risk_id = "R001", description = "Test",
    probability = 4, impact = 5, detectability = 2
  )
  rr <- create_risk_register(risks)
  ev <- risk_register_to_evidence(rr, ctx = ctx)

  item_row <- ev[ev$indicator_id == "RISK_ITEM", ]
  expect_equal(item_row$metric_value[1], 40)  # 4*5*2
})
