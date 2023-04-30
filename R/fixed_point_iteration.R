#' 2D fixed-point iteration
#'
#' @import pracma
#' @import cli
#'
#' @param f A function taking the value function and the elements of x and y
#' as input
#' @param x,y Vectors spanning the grid on which to approximate f
#' @param ... Arguments supplied to `f`
#' @param max_iterations Maximum number of iterations
#' @param abs_tolerance Convergence criterion. If the average absolute change
#' between two consecutive iterations is less than `abs_tolerance` the fix-point
#' iteration is stopped.
#'
#' @export
fixed_point_iteration_2d <- function(
    f, x, y, ..., max_iterations = 1000L, abs_tolerance = 1e-5
) {
  n <- length(x)
  m <- length(y)

  v_current <- v_new <- 1 * pracma::ones(m, n)
  v_history <- list()
  v_history[[1]] <- list(v_current)

  value_function <- function(x_t, y_t) {
    pracma::interp2(x, y, v_current, x_t, y_t)
  }

  for (k in seq_len(max_iterations)) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        v_new[j,i] <- f(value_function, x[i], y[j], ...)
      }
    }
    avg_error <- 1 / (n * m) * sum(abs(v_current - v_new))
    cli::cli_alert_info("Iteration {k} - avg. abs. error: {avg_error}")
    cli::cli_alert_info("Average value: {1 / (n * m) * sum(v_current)}")
    v_current <- v_new
    v_history <- c(v_history, list(v_new))
    if (avg_error < abs_tolerance) break
  }

  list(
    x = x,
    y = y,
    v = v_current,
    v_history = v_history
  )
}



#' Fixed-point iteration for wealth-consumption ratio
#'
#' @inheritParams pi_t1
#'
#' @export
fixed_point_iteration_pc <- function(
    n_lambda = 50,
    n_pi = 20,
    params = ghaderi_params(),
    ...
) {
  lambda <- pracma::linspace(log(0.005), log(11), n_lambda)
  lambda <- exp(lambda)
  pi <- linspace(0, 1, n_pi)

  fixed_point_iteration_2d(
    f = pc_t,
    x = lambda,
    y = pi,
    params = params,
    ...
  )
}



#' Fixed-point iteration for price-dividend ratio
#'
#' @param pc_grid Output of [fixed_point_iteration_pc].
#' @inheritParams pi_t1
#'
#' @export
fixed_point_iteration_pd <- function(
    pc_grid,
    n_lambda = 50,
    n_pi = 20,
    params = ghaderi_params()
) {
  lambda <- pracma::linspace(-10, log(11), n_lambda)
  lambda <- exp(lambda)
  pi <- pracma::linspace(0, 1, n_pi)

  fixed_point_iteration_2d(
    f = pd_t,
    x = lambda,
    y = pi,
    pc_grid = pc_grid,
    params = params
  )
}
