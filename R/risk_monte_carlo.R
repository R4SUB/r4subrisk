#' Monte Carlo Uncertainty on Risk Priority Numbers
#'
#' An FMEA scores each risk with single integer values for probability, impact,
#' and detectability, but those assessments are judgements with uncertainty. This
#' simulation treats each score as uncertain, draws many possible registers, and
#' reports the resulting distribution of Risk Priority Numbers, both per risk and
#' for the register as a whole. It turns a single RPN into a range and a
#' probability of being critical, which is what a risk review actually wants to
#' know.
#'
#' @details
#' For each draw, every FMEA component is sampled from a normal distribution
#' centred on its assessed value with standard deviation `spread` (in scale
#' points), then rounded and clamped to the 1--5 scale. `spread = 0` reproduces
#' the deterministic RPN. The per-draw RPN is `probability * impact *
#' detectability`, and the register total is their sum. The critical threshold is
#' the lower bound of the top RPN band in `config`.
#'
#' @param register A `risk_register` from [create_risk_register()], or a
#'   data.frame with `risk_id`, `probability`, `impact`, and `detectability`.
#' @param n Number of Monte Carlo iterations. Default `10000`.
#' @param spread Standard deviation of the per-component uncertainty, in scale
#'   points. Default `1`. Use `0` for a deterministic result.
#' @param config A `risk_config` from [risk_config_default()], used for the
#'   critical threshold.
#' @param seed Optional integer seed. When supplied the random state is set for
#'   the call and restored on exit, so the result is reproducible without
#'   disturbing the caller's RNG stream.
#'
#' @return A list of class `"risk_monte_carlo"` with `n`, `spread`,
#'   `crit_threshold`, `point_total` (the deterministic register RPN),
#'   `total` (named numeric summary of the total RPN distribution),
#'   `total_draws` (the simulated totals, for plotting), and `per_risk`
#'   (a data.frame with `risk_id`, `rpn`, `mc_mean`, `mc_sd`, `p05`, `p50`,
#'   `p95`, and `prob_critical`).
#'
#' @examples
#' risks <- data.frame(
#'   risk_id = c("R001", "R002", "R003"),
#'   description = c("Missing SDTM vars", "Unmapped derivations", "Define drift"),
#'   probability = c(4, 3, 2), impact = c(5, 4, 3), detectability = c(2, 3, 4)
#' )
#' rr <- suppressMessages(create_risk_register(risks))
#' mc <- risk_monte_carlo(rr, n = 2000, seed = 1)
#' mc$total
#' mc$per_risk
#'
#' @importFrom stats rnorm quantile sd
#' @export
risk_monte_carlo <- function(register, n = 10000, spread = 1,
                             config = risk_config_default(), seed = NULL) {
  if (!is.data.frame(register)) {
    cli::cli_abort("{.arg register} must be a risk register or data.frame.")
  }
  needed <- c("risk_id", "probability", "impact", "detectability")
  missing_cols <- setdiff(needed, names(register))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("{.arg register} is missing column(s): {.val {missing_cols}}.")
  }
  if (nrow(register) == 0L) {
    cli::cli_abort("{.arg register} has no risks to simulate.")
  }
  if (!is.numeric(n) || length(n) != 1L || n < 1) {
    cli::cli_abort("{.arg n} must be a positive integer.")
  }
  n <- as.integer(n)
  if (!is.numeric(spread) || length(spread) != 1L || spread < 0) {
    cli::cli_abort("{.arg spread} must be a non-negative number.")
  }

  # Reproducible without clobbering the caller's RNG stream.
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          rm(".Random.seed", envir = .GlobalEnv),
        add = TRUE
      )
    }
    set.seed(seed)
  }

  # Critical threshold: lower bound of the top RPN band.
  bands <- config$rpn_bands
  top_band <- names(bands)[which.max(vapply(bands, `[`, numeric(1), 2L))]
  crit_threshold <- bands[[top_band]][1]

  draw <- function(value) {
    if (spread == 0) return(rep(value, n))
    pmin(pmax(round(stats::rnorm(n, mean = value, sd = spread)), 1), 5)
  }
  qs <- function(x) stats::quantile(x, c(0.05, 0.5, 0.95), names = FALSE, type = 7)

  k <- nrow(register)
  total_draws <- numeric(n)
  per <- vector("list", k)
  for (i in seq_len(k)) {
    rpn_i <- draw(register$probability[i]) *
             draw(register$impact[i]) *
             draw(register$detectability[i])
    total_draws <- total_draws + rpn_i
    q <- qs(rpn_i)
    per[[i]] <- data.frame(
      risk_id       = as.character(register$risk_id[i]),
      rpn           = register$probability[i] * register$impact[i] *
                        register$detectability[i],
      mc_mean       = round(mean(rpn_i), 1),
      mc_sd         = round(stats::sd(rpn_i), 1),
      p05           = q[1],
      p50           = q[2],
      p95           = q[3],
      prob_critical = round(mean(rpn_i >= crit_threshold), 3),
      stringsAsFactors = FALSE
    )
  }
  per_risk <- do.call(rbind, per)
  per_risk <- per_risk[order(-per_risk$prob_critical, -per_risk$mc_mean), ,
                       drop = FALSE]
  rownames(per_risk) <- NULL

  qt <- qs(total_draws)
  total <- c(
    mean = round(mean(total_draws), 1),
    sd   = round(stats::sd(total_draws), 1),
    p05  = qt[1], p50 = qt[2], p95 = qt[3],
    min  = min(total_draws), max = max(total_draws)
  )

  structure(
    list(
      n              = n,
      spread         = spread,
      crit_threshold = crit_threshold,
      point_total    = sum(per_risk$rpn),
      total          = total,
      total_draws    = total_draws,
      per_risk       = per_risk
    ),
    class = "risk_monte_carlo"
  )
}

#' Print Monte Carlo Risk Result
#' @param x A `risk_monte_carlo` object.
#' @param ... Ignored.
#' @export
print.risk_monte_carlo <- function(x, ...) {
  cli::cli_alert_info(
    "Monte Carlo RPN: {x$n} draws, spread {x$spread} scale point{?s}."
  )
  cli::cli_alert_info(
    "Total RPN: mean {.val {unname(x$total[['mean']])}} (90% CI {unname(x$total[['p05']])}-{unname(x$total[['p95']])}); point estimate {.val {x$point_total}}."
  )
  n_likely <- sum(x$per_risk$prob_critical >= 0.5)
  cli::cli_alert_info(
    "{n_likely} risk{?s} more likely than not to be critical (RPN >= {x$crit_threshold})."
  )
  invisible(x)
}
