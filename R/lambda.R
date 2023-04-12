#' PDF of lambda_t1
#'
#' @export
pdf_lambda_t1 <- function(s_t1, lambda_t, params = ghaderi_params()) {
  r_l <- params$rho_lambda
  s_l <- params$sigma_lambda
  lrm_lambda_t1 <- params$long_run_mean_lambda_t1(s_t1)

  mean_lambda_t1 <- (1 - r_l) * lrm_lambda_t1 + r_l * lambda_t
  sd_lambda_t1 <- s_l * sqrt(lambda_t)

  function(lambda_t1) {
    dnorm(
      x = lambda_t1,
      mean = mean_lambda_t1,
      sd = sd_lambda_t1
    )
  }
}



#' Quantiles of lambda_t1
#'
#' @export
q_lambda_t1 <- function(p, s_t1, lambda_t, params = ghaderi_params()) {
  r_l <- params$rho_lambda
  s_l <- params$sigma_lambda
  lrm_lambda_t1 <- params$long_run_mean_lambda_t1(s_t1)

  mean_lambda_t1 <- (1 - r_l) * lrm_lambda_t1 + r_l * lambda_t
  sd_lambda_t1 <- s_l * sqrt(lambda_t)

  qnorm(p, mean = mean_lambda_t1, sd = sd_lambda_t1)
}
