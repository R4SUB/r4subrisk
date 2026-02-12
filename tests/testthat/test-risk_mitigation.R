test_that("apply_mitigations updates probability and recomputes RPN", {
  rr <- create_risk_register(make_test_risks())
  old_rpn <- rr$rpn[rr$risk_id == "R001"]

  updates <- data.frame(
    risk_id     = "R001",
    probability = 2,
    status      = "mitigated"
  )
  rr2 <- apply_mitigations(rr, updates)

  expect_s3_class(rr2, "risk_register")
  expect_equal(rr2$probability[rr2$risk_id == "R001"], 2)
  expect_equal(rr2$status[rr2$risk_id == "R001"], "mitigated")
  # RPN should decrease
  expect_true(rr2$rpn[rr2$risk_id == "R001"] < old_rpn)
})

test_that("apply_mitigations updates mitigation notes", {
  rr <- create_risk_register(make_test_risks())
  updates <- data.frame(
    risk_id    = "R002",
    mitigation = "Added validation check"
  )
  rr2 <- apply_mitigations(rr, updates)
  expect_equal(rr2$mitigation[rr2$risk_id == "R002"], "Added validation check")
})

test_that("apply_mitigations warns on unknown risk_ids", {
  rr <- create_risk_register(make_test_risks())
  updates <- data.frame(risk_id = "RZZZ", probability = 1)
  expect_warning(
    apply_mitigations(rr, updates),
    "unknown risk_id"
  )
})

test_that("apply_mitigations rejects missing risk_id column", {
  rr <- create_risk_register(make_test_risks())
  expect_error(
    apply_mitigations(rr, data.frame(probability = 1)),
    "risk_id"
  )
})

test_that("apply_mitigations does not affect non-updated risks", {
  rr <- create_risk_register(make_test_risks())
  old_rpn_r002 <- rr$rpn[rr$risk_id == "R002"]

  updates <- data.frame(risk_id = "R001", probability = 1)
  rr2 <- apply_mitigations(rr, updates)

  expect_equal(rr2$rpn[rr2$risk_id == "R002"], old_rpn_r002)
})

test_that("compare_risk_registers detects new and resolved risks", {
  r1 <- data.frame(
    risk_id = c("R001", "R002"),
    description = c("A", "B"),
    probability = c(4, 3), impact = c(5, 4), detectability = c(2, 3)
  )
  r2 <- data.frame(
    risk_id = c("R001", "R003"),
    description = c("A", "C"),
    probability = c(2, 3), impact = c(5, 3), detectability = c(2, 2)
  )
  rr1 <- create_risk_register(r1)
  rr2 <- create_risk_register(r2)

  comp <- compare_risk_registers(rr1, rr2)

  expect_equal(comp$new_risks, "R003")
  expect_equal(comp$resolved_risks, "R002")
})

test_that("compare_risk_registers computes RPN delta", {
  r1 <- data.frame(
    risk_id = "R001", description = "A",
    probability = 4, impact = 5, detectability = 2
  )
  r2 <- data.frame(
    risk_id = "R001", description = "A",
    probability = 2, impact = 5, detectability = 2
  )
  rr1 <- create_risk_register(r1)
  rr2 <- create_risk_register(r2)

  comp <- compare_risk_registers(rr1, rr2)

  expect_equal(nrow(comp$rpn_changes), 1)
  expect_true(comp$rpn_changes$rpn_delta[1] < 0)  # RPN decreased
  expect_true(comp$delta_mean_rpn < 0)
})

test_that("compare_risk_registers detects level transitions", {
  r1 <- data.frame(
    risk_id = "R001", description = "A",
    probability = 5, impact = 5, detectability = 5  # RPN=125, critical
  )
  r2 <- data.frame(
    risk_id = "R001", description = "A",
    probability = 1, impact = 1, detectability = 1  # RPN=1, low
  )
  rr1 <- create_risk_register(r1)
  rr2 <- create_risk_register(r2)

  comp <- compare_risk_registers(rr1, rr2)

  expect_true(comp$level_transitions$changed[1])
  expect_equal(comp$level_transitions$level_before[1], "critical")
  expect_equal(comp$level_transitions$level_after[1], "low")
})
