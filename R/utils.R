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
    pracma::interp2(grid$x, grid$y, grid$v, lambda_t, pi_t)
  }
}



#' Compute annual growth from monthly growth observations
#'
#' @param monthly_growth Vector of monthly growth observations where 1 means
#' no growth.
#'
#' @import purrr
#'
#' @export
annual_growth <- function(monthly_growth) {
  n <- length(monthly_growth)
  stopifnot((length(monthly_growth) %% 12) == 0)
  year_chunks <- split(monthly_growth, ceiling(seq_len(n) / 12))
  purrr::map_dbl(year_chunks, function(chunk) {
    prod(chunk)
  })
}



#' Replace NA with nearest non-NA value
#'
#' @export
replace_na_with_nearest <- function(y, x) {
  if (!any(is.na(y))) return(y)

  ord <- order(x)
  y_ord <- y[ord]
  x_ord <- x[ord]

  na_indices <- which(is.na(y_ord))
  max_na_index <- max(na_indices)

  if (max_na_index == length(y)) {
    # NAs for large y / x
    first_non_na_index <- min(na_indices) - 1

    dy <- y_ord[first_non_na_index] - y_ord[first_non_na_index - 1]
    dx <- x_ord[first_non_na_index] - x_ord[first_non_na_index - 1]

    y_ord[na_indices] <- y_ord[first_non_na_index] +
      dy / dx * (x_ord[na_indices] - x_ord[first_non_na_index])
  } else {
    # NAs for small y / x
    first_non_na_index <- max_na_index + 1

    dy <- y_ord[first_non_na_index + 1] - y_ord[first_non_na_index]
    dx <- x_ord[first_non_na_index + 1] - x_ord[first_non_na_index]

    y_ord[na_indices] <- y_ord[first_non_na_index] +
      dy / dx * (x_ord[na_indices] - x_ord[first_non_na_index])
  }

  y_ord[order(ord)]
}
