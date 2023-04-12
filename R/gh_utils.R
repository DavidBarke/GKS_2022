#' Convert \eqn{(\lambda, \pi)} grid to function \eqn{f(\lambda, \pi)}
#'
#' @param grid A grid returned by [fpi_2d].
#'
#' @returns A function taking `lambda_t` and `pi_t` as input. Calling this
#' function returns the interpolated value over the previously supplied `grid`.
#'
#' @export
grid_to_function <- function(grid) {
  force(grid)
  function(lambda_t, pi_t) {
    pracma::interp2(pc_grid$x, pc_grid$y, pc_grid$v, lambda_t, pi_t)
  }
}
