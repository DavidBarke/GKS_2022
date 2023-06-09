% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params.R
\name{ghaderi_params}
\alias{ghaderi_params}
\title{Parameters for Ghaderi et al. (2022)}
\usage{
ghaderi_params(
  gamma = 5,
  psi = 1.5,
  delta = 0.999,
  mu_c = 0.0026,
  sigma_c = 0.002,
  phi = 3,
  rho_lambda = 0.9933,
  sigma_lambda = 0.0083,
  lambda_L = 0.0417,
  lambda_H = 0.4167,
  mu_Z = 0.02,
  p_01 = 0.0017,
  p_10 = 0.0208
)
}
\arguments{
\item{gamma}{Relative risk aversion}

\item{psi}{Elasticity of intertemporal substitution}

\item{delta}{Time discount}

\item{mu_c}{Mean consumption growth in the absence of jumps}

\item{sigma_c}{Volatility of consumption growth in the absence of jumps}

\item{phi}{Leverage parameter}

\item{sigma_lambda}{Conditional volatility of shocks to jump intensity}

\item{lambda_L}{Average jump intensity in normal times}

\item{lambda_H}{Average jump intensity in depression times}

\item{mu_Z}{Mean jump size}

\item{p_01}{Transition probability from normal state to depression state}

\item{p_10}{Transition probability from depression state to normal state}
}
\description{
Parameters for Ghaderi et al. (2022)
}
