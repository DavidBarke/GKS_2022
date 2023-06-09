% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixed_point_iteration.R
\name{fixed_point_iteration_pd}
\alias{fixed_point_iteration_pd}
\title{Fixed-point iteration for price-dividend ratio}
\usage{
fixed_point_iteration_pd(
  pc_grid,
  n_lambda = 50,
  n_pi = 20,
  params = ghaderi_params()
)
}
\arguments{
\item{pc_grid}{Output of \link{fixed_point_iteration_pc}.}

\item{params}{List of parameters as returned by \link{ghaderi_params}}
}
\description{
Fixed-point iteration for price-dividend ratio
}
