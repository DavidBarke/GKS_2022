table_V <- function(params = model_params()) {
  list(
    expected_equity_rate = expected_equity_rate(params),
    expected_bill_rate = expected_bill_rate(params),
    equity_premium = equity_premium(params),
    expected_equity_rate_conditional = expected_equity_rate(
      params, conditional = TRUE
    ),
    face_bill_rate = face_bill_rate(
      params
    ),
    equity_premium_conditional = equity_premium(
      params, conditional = TRUE
    ),
    price_earnings_ratio = price_earnings_ratio(params),
    expected_growth_rate = expected_growth_rate(params),
    expected_growth_rate_conditional = expected_growth_rate(
      params, conditional = TRUE
    ),
    expected_equity_rate_levered = expected_equity_rate(
      params, levered = TRUE
    ),
    equity_premium_levered = equity_premium(
      params, levered = TRUE
    ),
    expected_equity_rate_levered_conditional = expected_equity_rate(
      params, conditional = TRUE, levered = TRUE
    ),
    equity_premium_levered_conditional = expected_equity_rate(
      params, conditional = TRUE, levered = TRUE
    )
  )
}

#' Eq. 9 + 10
expected_equity_rate <- function(
    params = model_params(), conditional = FALSE, levered = FALSE,
    trend_adjusted = FALSE
) {
  x <- params$rho +
    params$theta * params$gamma -
    1/2 * params$theta^2 * params$sigma^2 +
    params$theta * params$sigma^2 -
    params$p * (
      E_b_trans(function(b) (1 - b)^(1 - params$theta), trend_adjusted) -
        1 + E_b_trans(identity, trend_adjusted)
    )

  if (conditional) {
    x <- x + params$p * E_b_trans(identity, trend_adjusted)
  }

  x
}

#' Eq. 11
face_bill_rate <- function(params = model_params()) {
  expected_bill_rate(params) + params$p * params$q * E_b_trans(identity)
}

#' Eq. 12
expected_bill_rate <- function(params = model_params()) {
  params$rho +
    params$theta * params$gamma -
    1/2 * params$theta^2 * params$sigma^2 -
    params$p * (
      (1 - params$q) * E_b_trans(function(b) (1 - b)^(-params$theta)) +
        params$q * E_b_trans(function(b) (1 - b)^(1 - params$theta)) +
        params$q * E_b_trans(identity) - 1
    )
}

#' Eq. 13 + 14
equity_premium <- function(
    params = model_params(), conditional = FALSE, levered = FALSE
) {
  if (conditional) {
    expected_equity_rate(params, conditional) - face_bill_rate(params)
  } else {
    expected_equity_rate(params) - expected_bill_rate(params)
  }
}

#' Eq. 17
price_earnings_ratio <- function(params = model_params()) {
  (
    params$rho +
      (params$theta - 1) * params$gamma -
      1/2 * (params$theta - 1)^2 * params$sigma^2 -
      params$p * (
        E_b_trans(function(b) (1 - b)^(1 - params$theta)) - 1
      )
  )^-1
}

#' Eq. 18 + 19
expected_growth_rate <- function(params = model_params(), conditional = FALSE) {
  if (conditional) {
    params$gamma +
      1/2 * params$sigma^2
  } else {
    params$gamma +
      1/2 * params$sigma^2 -
      params$p * E_b_trans(identity)
  }
}
