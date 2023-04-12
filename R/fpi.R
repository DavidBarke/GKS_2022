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
fpi_2d <- function(
    f, x, y, ..., max_iterations = 100L, abs_tolerance = 1e-4
) {
  n <- length(x)
  m <- length(y)
  mgrid <- pracma::meshgrid(x, y)

  v_current <- v_new <- pracma::ones(n, m)

  value_function <- function(x_t, y_t) {
    pracma::interp2(x, y, v_current, x_t, y_t)
  }

  for (k in seq_len(max_iterations)) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        v_new[i,j] <- f(value_function, x[i], y[j], ...)
      }
    }
    avg_error <- 1 / (n * m) * sum(abs(v_current - v_new))
    cli::cli_alert_info("Iteration {k} - avg. abs. error: {avg_error}")
    v_current <- v_new
    if (avg_error < abs_tolerance) break
  }

  list(
    x = x,
    y = y,
    v = v_current
  )
}



#' Fixed-point iteration for wealth-consumption ratio
#'
#' @inheritParams pi_t1
#'
#' @export
fpi_pc <- function(params = ghaderi_params()) {
  lambda <- pracma::linspace(0.038, 0.44, 50)
  pi <- pracma::linspace(0, 1, 50)

  fpi_2d(
    f = pc_t,
    x = lambda,
    y = pi,
    params = params
  )
}



#' Fixed-point iteration for price-dividend ratio
#'
#' @param pc_grid Output of [fpi_pc].
#' @inheritParams pi_t1
#'
#' @export
fpi_pd <- function(pc_grid, params = ghaderi_params()) {
  lambda <- pracma::linspace(0.038, 0.44, 50)
  pi <- pracma::linspace(0, 1, 50)

  fpi_2d(
    f = pd_t,
    x = lambda,
    y = pi,
    pc_grid = pc_grid,
    params = params
  )
}
