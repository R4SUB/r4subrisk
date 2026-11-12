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

# --- Additional round-4 coverage -------------------------------------------

# A register whose RPNs are 50, 30, 20 (total 100), so the cumulative shares
# land on the clean breakpoints 0.5, 0.8, 1.0.
make_breakpoint_register <- function() {
  create_risk_register(data.frame(
    risk_id       = c("A", "B", "C"),
    description   = c("a", "b", "c"),
    probability   = c(5, 5, 4),
    impact        = c(5, 3, 5),
    detectability = c(2, 2, 1),
    stringsAsFactors = FALSE
  ))
}

test_that("cum_cutoff near zero flags only the single top risk", {
  pr <- risk_pareto(make_breakpoint_register(), cum_cutoff = 0.001)
  # Even a tiny cutoff is reached by the first (largest) risk alone.
  expect_equal(pr$vital_few, c(TRUE, FALSE, FALSE))
  expect_equal(attr(pr, "n_vital_few"), 1L)
})

test_that("a cutoff that equals a cumulative-share breakpoint is inclusive", {
  rr <- make_breakpoint_register()
  pr <- risk_pareto(rr, cum_cutoff = 0.8)
  # cum_share is exactly 0.5, 0.8, 1.0; the >= 0.8 test includes the 2nd risk.
  expect_equal(pr$cum_share, c(0.5, 0.8, 1))
  expect_equal(pr$vital_few, c(TRUE, TRUE, FALSE))
  expect_equal(attr(pr, "n_vital_few"), 2L)

  # Nudging the cutoff just past the breakpoint pulls in the next risk.
  pr_eps <- risk_pareto(rr, cum_cutoff = 0.80001)
  expect_equal(pr_eps$vital_few, c(TRUE, TRUE, TRUE))
  expect_equal(attr(pr_eps, "n_vital_few"), 3L)
})

test_that("cum_cutoff of 1 flags every risk and the final cum_share is 1", {
  rr <- make_breakpoint_register()
  pr <- risk_pareto(rr, cum_cutoff = 1)
  expect_true(all(pr$vital_few))
  expect_equal(attr(pr, "n_vital_few"), nrow(rr))
  expect_equal(pr$cum_share[nrow(pr)], 1)
})

test_that("all-equal RPN risks share rank equally and tie-break on risk_id", {
  rr <- create_risk_register(data.frame(
    risk_id       = c("E2", "E4", "E1", "E3"),
    description   = letters[1:4],
    probability   = c(3, 3, 3, 3),
    impact        = c(3, 3, 3, 3),
    detectability = c(3, 3, 3, 3),
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr, cum_cutoff = 0.8)
  # Order is fully determined by risk_id since every RPN is identical.
  expect_equal(pr$risk_id, c("E1", "E2", "E3", "E4"))
  expect_true(all(pr$rpn == 27))
  expect_true(all(pr$rpn_share == 0.25))
  expect_equal(pr$cum_share, c(0.25, 0.5, 0.75, 1))
  # 0.8 is only reached by the full set.
  expect_true(all(pr$vital_few))
})

test_that("a single-risk register carries the whole register's RPN", {
  rr <- create_risk_register(data.frame(
    risk_id = "S1", description = "only risk",
    probability = 4, impact = 4, detectability = 2,
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr, cum_cutoff = 0.3)
  expect_equal(nrow(pr), 1L)
  expect_equal(pr$rpn_share, 1)
  expect_equal(pr$cum_share, 1)
  # The lone risk always meets any cutoff in (0, 1].
  expect_true(pr$vital_few)
  expect_equal(attr(pr, "n_vital_few"), 1L)
})

test_that("the vital-few cut can split a group of tied RPNs", {
  # RPNs 30, 30, 30, 10 (total 100); cumulative shares 0.3, 0.6, 0.9, 1.0.
  rr <- create_risk_register(data.frame(
    risk_id       = c("T3", "T1", "T2", "T4"),
    description   = c("c", "a", "b", "d"),
    probability   = c(5, 5, 5, 5),
    impact        = c(3, 3, 3, 2),
    detectability = c(2, 2, 2, 1),
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr, cum_cutoff = 0.5)
  expect_equal(pr$risk_id, c("T1", "T2", "T3", "T4"))
  expect_equal(pr$rpn, c(30, 30, 30, 10))
  # 0.5 is first reached at the 2nd risk, so only 2 of the 3 tied 30s flag,
  # and the deterministic risk_id order decides which two.
  expect_equal(pr$vital_few, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(attr(pr, "n_vital_few"), 2L)
})

test_that("n_vital_few is monotonic non-decreasing in cum_cutoff", {
  rr <- create_risk_register(make_test_risks())
  cutoffs <- seq(0.1, 1, by = 0.1)
  n_vf <- vapply(
    cutoffs,
    function(c) attr(risk_pareto(rr, cum_cutoff = c), "n_vital_few"),
    integer(1)
  )
  expect_true(all(diff(n_vf) >= 0))
  # the sweep must span from "some flagged" up to "all flagged"
  expect_gte(min(n_vf), 1L)
  expect_equal(max(n_vf), nrow(rr))
})

test_that("risk_level is passed through unchanged from the register", {
  rr <- create_risk_register(make_test_risks())
  pr <- risk_pareto(rr)
  # Map each ranked risk back to its register risk_level; they must match.
  expected <- rr$risk_level[match(pr$risk_id, rr$risk_id)]
  expect_equal(pr$risk_level, expected)
  # No recomputation: the set of levels is exactly the register's.
  expect_setequal(unique(pr$risk_level), unique(rr$risk_level))
})

test_that("empty (zero-total) register flags nothing and keeps the cutoff", {
  rr <- create_risk_register(data.frame(
    risk_id = character(0), description = character(0),
    stringsAsFactors = FALSE
  ))
  pr <- risk_pareto(rr, cum_cutoff = 0.5)
  expect_equal(attr(pr, "total_rpn"), 0)
  expect_equal(attr(pr, "n_vital_few"), 0L)
  # The caller-supplied cutoff is still recorded on the empty result.
  expect_equal(attr(pr, "cum_cutoff"), 0.5)
  expect_type(pr$vital_few, "logical")
  expect_type(pr$rpn_share, "double")
})

test_that("invalid cum_cutoff values are all rejected", {
  rr <- create_risk_register(make_test_risks())
  expect_error(risk_pareto(rr, cum_cutoff = NA_real_), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = -0.1), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = Inf), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = "0.8"), "cum_cutoff")
  expect_error(risk_pareto(rr, cum_cutoff = numeric(0)), "cum_cutoff")
})

test_that("plot_risk_pareto builds the expected layered structure", {
  skip_if_not_installed("ggplot2")
  rr <- make_breakpoint_register()
  p <- plot_risk_pareto(rr, cum_cutoff = 0.8)

  # bars + cumulative line + points + cutoff reference line
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  expect_equal(geoms, c("GeomCol", "GeomLine", "GeomPoint", "GeomHline"))

  # x axis is the ranked risk_id factor, in descending-RPN order
  expect_true(is.factor(p$data$risk_id))
  expect_equal(levels(p$data$risk_id), c("A", "B", "C"))

  # cumulative share is scaled onto the RPN axis for the overlay
  expect_true("cum_scaled" %in% names(p$data))
  expect_equal(p$data$cum_scaled, p$data$cum_share * max(p$data$rpn))

  # a fill scale (vital few vs trivial many) and a secondary axis are present
  expect_false(is.null(p$scales$get_scales("fill")))
  expect_false(is.null(p$scales$get_scales("y")$secondary.axis))
  expect_equal(p$labels$title, "Risk Pareto")
})
