# Tests for write_risk_register / read_risk_register (R/risk_register_io.R)

test_that("write_risk_register writes a CSV and returns the path invisibly", {
  rr <- create_risk_register(make_test_risks())
  path <- tempfile(fileext = ".csv")

  res <- withVisible(write_risk_register(rr, path))
  expect_false(res$visible)
  expect_equal(res$value, path)
  expect_true(file.exists(path))

  # the file is a faithful, human-readable snapshot incl. computed columns
  on_disk <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_true(all(c("risk_id", "description", "rpn", "risk_level") %in%
                    names(on_disk)))
  expect_equal(nrow(on_disk), nrow(rr))
})

test_that("CSV write -> read round-trip reproduces an equivalent register", {
  rr <- create_risk_register(make_test_risks())
  path <- tempfile(fileext = ".csv")

  write_risk_register(rr, path)
  rr2 <- read_risk_register(path)

  expect_s3_class(rr2, "risk_register")
  expect_equal(rr2, rr)
})

test_that("round-trip holds when owner/mitigation/status are populated", {
  risks <- make_test_risks()
  risks$owner      <- c("alice", "bob", NA, "dana")
  risks$mitigation <- c("remap", NA, "re-run define", NA)
  risks$status     <- c("open", "closed", "open", "mitigated")
  rr <- create_risk_register(risks)

  path <- tempfile(fileext = ".csv")
  write_risk_register(rr, path)
  rr2 <- read_risk_register(path)

  expect_equal(rr2, rr)
})

test_that("read_risk_register recomputes RPN rather than trusting the file", {
  rr <- create_risk_register(make_test_risks())
  path <- tempfile(fileext = ".csv")
  write_risk_register(rr, path)

  # Corrupt the stored computed columns on disk.
  on_disk <- utils::read.csv(path, stringsAsFactors = FALSE)
  on_disk$rpn <- 999
  on_disk$risk_level <- "bogus"
  utils::write.csv(on_disk, path, row.names = FALSE, na = "NA")

  rr2 <- read_risk_register(path)
  expect_equal(rr2$rpn, rr$rpn)
  expect_equal(rr2$risk_level, rr$risk_level)
  expect_false(any(rr2$risk_level == "bogus"))
})

test_that("read_risk_register rebuilds schema from a minimal file", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(risk_id = c("R001", "R002"),
               description = c("A", "B"),
               stringsAsFactors = FALSE),
    path, row.names = FALSE
  )

  rr <- read_risk_register(path)
  expect_s3_class(rr, "risk_register")
  # defaults filled by create_risk_register()
  expect_equal(rr$category, c("general", "general"))
  expect_equal(rr$status, c("open", "open"))
  expect_true(all(c("rpn", "risk_level") %in% names(rr)))
})

test_that("an empty register round-trips", {
  empty <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  path <- tempfile(fileext = ".csv")
  write_risk_register(empty, path)
  rr2 <- read_risk_register(path)

  expect_s3_class(rr2, "risk_register")
  expect_equal(nrow(rr2), 0L)
})

test_that("read_risk_register routes through create_risk_register validation", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(description = "no id", stringsAsFactors = FALSE),
    path, row.names = FALSE
  )
  expect_error(read_risk_register(path), "missing required column")
})

test_that("invalid inputs are rejected", {
  rr <- create_risk_register(make_test_risks())
  expect_error(write_risk_register(data.frame(a = 1), "x.csv"),
               "must be a")
  expect_error(write_risk_register(rr, ""), "non-empty file path")
  expect_error(read_risk_register(character(0)), "non-empty file path")
  expect_error(read_risk_register(file.path(tempdir(), "no_such_file.csv")),
               "not found")
})

test_that("xlsx write -> read round-trip reproduces an equivalent register", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  rr <- create_risk_register(make_test_risks())
  path <- tempfile(fileext = ".xlsx")

  write_risk_register(rr, path)
  expect_true(file.exists(path))

  rr2 <- read_risk_register(path)
  expect_s3_class(rr2, "risk_register")
  expect_equal(rr2$risk_id, rr$risk_id)
  expect_equal(rr2$rpn, rr$rpn)
  expect_equal(rr2$risk_level, rr$risk_level)
})
