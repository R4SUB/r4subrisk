# Tests for reconcile_risk_registers() (R/reconcile_risk_registers.R)

make_prior <- function() {
  create_risk_register(data.frame(
    risk_id       = c("R001", "R002", "R003"),
    description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                      "Inconsistent define.xml"),
    probability   = c(4, 3, 2),
    impact        = c(5, 4, 3),
    detectability = c(2, 3, 4),
    status        = c("open", "open", "open"),
    stringsAsFactors = FALSE
  ))
}

# R001: probability 4 -> 2 (changed). R002: identical (unchanged).
# R003: dropped (removed). R004: new (added).
make_current <- function() {
  create_risk_register(data.frame(
    risk_id       = c("R001", "R002", "R004"),
    description   = c("Missing SDTM variables", "Unmapped ADaM derivations",
                      "Late-breaking lab data"),
    probability   = c(2, 3, 3),
    impact        = c(5, 4, 4),
    detectability = c(2, 3, 2),
    status        = c("open", "open", "open"),
    stringsAsFactors = FALSE
  ))
}

test_that("reconcile classifies added, removed, changed, unchanged", {
  rec <- reconcile_risk_registers(make_prior(), make_current())

  expect_s3_class(rec, "risk_reconciliation")
  types <- stats::setNames(rec$per_risk$change_type, rec$per_risk$risk_id)
  expect_equal(types[["R001"]], "changed")
  expect_equal(types[["R002"]], "unchanged")
  expect_equal(types[["R003"]], "removed")
  expect_equal(types[["R004"]], "added")

  expect_equal(rec$n_added, 1L)
  expect_equal(rec$n_removed, 1L)
  expect_equal(rec$n_changed, 1L)
  expect_equal(rec$n_unchanged, 1L)
  expect_equal(rec$n_prior, 3L)
  expect_equal(rec$n_current, 3L)
})

test_that("per_risk covers the union of risk_ids exactly once", {
  rec <- reconcile_risk_registers(make_prior(), make_current())
  expect_setequal(rec$per_risk$risk_id, c("R001", "R002", "R003", "R004"))
  expect_equal(nrow(rec$per_risk), 4L)
  expect_false(any(duplicated(rec$per_risk$risk_id)))
})

test_that("field_changes lists the differing fields on changed risks only", {
  rec <- reconcile_risk_registers(make_prior(), make_current())

  # Only R001 changed, and only its probability differs.
  expect_equal(unique(rec$field_changes$risk_id), "R001")
  expect_equal(rec$field_changes$field, "probability")
  expect_equal(rec$field_changes$value_prior, "4")
  expect_equal(rec$field_changes$value_current, "2")

  r001 <- rec$per_risk[rec$per_risk$risk_id == "R001", ]
  expect_equal(r001$n_fields_changed, 1L)
  expect_equal(r001$changed_fields, "probability")
})

test_that("RPN movement is read from stored values, never recomputed", {
  rec <- reconcile_risk_registers(make_prior(), make_current())
  pr <- rec$per_risk

  # R001: 4*5*2=40 -> 2*5*2=20
  r001 <- pr[pr$risk_id == "R001", ]
  expect_equal(r001$rpn_prior, 40)
  expect_equal(r001$rpn_current, 20)
  expect_equal(r001$rpn_delta, -20)

  # Added risk has no prior RPN; removed risk has no current RPN.
  r004 <- pr[pr$risk_id == "R004", ]
  expect_true(is.na(r004$rpn_prior))
  expect_equal(r004$rpn_current, 24)
  expect_true(is.na(r004$rpn_delta))

  r003 <- pr[pr$risk_id == "R003", ]
  expect_equal(r003$rpn_prior, 24)
  expect_true(is.na(r003$rpn_current))
  expect_true(is.na(r003$rpn_delta))

  # Register-level movement: 100 -> 80.
  expect_equal(rec$total_rpn_prior, 100)
  expect_equal(rec$total_rpn_current, 80)
  expect_equal(rec$total_rpn_delta, -20)
})

test_that("unchanged NA fields are equal, NA-to-value counts as a change", {
  prior <- create_risk_register(data.frame(
    risk_id       = c("R001", "R002"),
    description   = c("A", "B"),
    probability   = c(3, 3), impact = c(3, 3), detectability = c(3, 3),
    stringsAsFactors = FALSE
  ))
  current <- create_risk_register(data.frame(
    risk_id       = c("R001", "R002"),
    description   = c("A", "B"),
    probability   = c(3, 3), impact = c(3, 3), detectability = c(3, 3),
    mitigation    = c(NA, "Added validation"),
    status        = c("open", "mitigated"),
    stringsAsFactors = FALSE
  ))

  rec <- reconcile_risk_registers(prior, current)
  types <- stats::setNames(rec$per_risk$change_type, rec$per_risk$risk_id)

  # R001: both mitigations NA, both status "open" -> unchanged.
  expect_equal(types[["R001"]], "unchanged")
  # R002: mitigation NA -> value, status open -> mitigated -> changed in 2 fields.
  expect_equal(types[["R002"]], "changed")

  r002 <- rec$per_risk[rec$per_risk$risk_id == "R002", ]
  expect_equal(r002$n_fields_changed, 2L)
  expect_setequal(
    strsplit(r002$changed_fields, ", ", fixed = TRUE)[[1]],
    c("mitigation", "status")
  )
  fc <- rec$field_changes[rec$field_changes$risk_id == "R002", ]
  mit <- fc[fc$field == "mitigation", ]
  expect_true(is.na(mit$value_prior))
  expect_equal(mit$value_current, "Added validation")
})

test_that("fields argument restricts what counts as a change", {
  prior   <- make_prior()
  current <- make_current()

  # Only consider status; R001's probability change is then ignored.
  rec <- reconcile_risk_registers(prior, current, fields = "status")
  types <- stats::setNames(rec$per_risk$change_type, rec$per_risk$risk_id)
  expect_equal(types[["R001"]], "unchanged")
  expect_equal(nrow(rec$field_changes), 0L)
})

test_that("per_risk is ordered changed, added, removed, unchanged", {
  rec <- reconcile_risk_registers(make_prior(), make_current())
  expect_equal(rec$per_risk$change_type,
               c("changed", "added", "removed", "unchanged"))
})

test_that("reconciling a register with itself reports all unchanged", {
  rr <- make_prior()
  rec <- reconcile_risk_registers(rr, rr)
  expect_true(all(rec$per_risk$change_type == "unchanged"))
  expect_equal(rec$n_changed, 0L)
  expect_equal(rec$total_rpn_delta, 0)
  expect_equal(nrow(rec$field_changes), 0L)
})

test_that("empty registers reconcile to an empty result", {
  empty <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  rec <- reconcile_risk_registers(empty, empty)
  expect_equal(nrow(rec$per_risk), 0L)
  expect_equal(nrow(rec$field_changes), 0L)
  expect_equal(rec$n_added, 0L)
  expect_equal(rec$total_rpn_delta, 0)
})

test_that("all-added and all-removed extremes are handled", {
  empty <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  rr <- make_prior()

  added <- reconcile_risk_registers(empty, rr)
  expect_true(all(added$per_risk$change_type == "added"))
  expect_equal(added$n_added, 3L)
  expect_equal(added$total_rpn_current, 100)

  removed <- reconcile_risk_registers(rr, empty)
  expect_true(all(removed$per_risk$change_type == "removed"))
  expect_equal(removed$n_removed, 3L)
  expect_equal(removed$total_rpn_prior, 100)
})

test_that("invalid inputs are rejected", {
  rr <- make_prior()
  expect_error(reconcile_risk_registers(data.frame(a = 1), rr),
               "must be")
  expect_error(reconcile_risk_registers(rr, data.frame(a = 1)),
               "must be")
  expect_error(reconcile_risk_registers(rr, rr, fields = character(0)),
               "non-empty")
  expect_error(reconcile_risk_registers(rr, rr, fields = "no_such_column"),
               "absent")
})

test_that("print returns its input invisibly", {
  rec <- reconcile_risk_registers(make_prior(), make_current())
  out <- withVisible(print(rec))
  expect_false(out$visible)
  expect_identical(out$value, rec)
})
