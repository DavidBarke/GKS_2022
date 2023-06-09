% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{grid_to_function}
\alias{grid_to_function}
\title{Convert \eqn{(\lambda, \pi)} grid to function \eqn{f(\lambda, \pi)}}
\usage{
grid_to_function(grid)
}
\arguments{
\item{grid}{A grid returned by \link{fpi_2d}.}
}
\value{
A function taking \code{lambda_t} and \code{pi_t} as input. Calling this
function returns the interpolated value over the previously supplied \code{grid}.
}
\description{
Convert \eqn{(\lambda, \pi)} grid to function \eqn{f(\lambda, \pi)}
}
