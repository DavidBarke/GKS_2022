S <- function(K = 1, b = 2) {
  K * b / (b - 1)
}

b <- function(r = 0.035, d = 0.03, s = 0.02) {
  - (r - d - 1/2 * s^2) / s^2 +
    sqrt(((d - r + 1/2 * s^2) / s^2)^2 + 2 * r / s^2)
}

ds_db <- function(K = 1, b = 2) {
  - K / (b - 1)^2
}

db_dr <- function(r = 0.035, d = 0.03, s = 0.02) {
  num <- 2 * (r - d) + s^2
  denom <- sqrt(4 * d^2 + 4 * d * (s^2 - 2 * r) + (s^2 + 2 * r)^2)

  1 / s^2 * (num /denom - 1)
}

db_dd <- function(r = 0.035, d = 0.03, s = 0.02) {
  num <- s^2 - 2 * (r - d)
  denom <- sqrt(4 * r^2 + 4 * r * (s^2 - 2 * d) + (s^2 + 2 * d)^2)

  1 / s^2 * (1 + num / denom)
}

db_ds <- function(r = 0.035, d = 0.03, s = 0.02) {
  num <- 2 * d^2 + d * (s^2 - 4 * r) + r * (2 * r + s^2)
  denom <- sqrt(4 * d^2 - 4 * d * (2 * r - s^2) + 4 * r^2 + 4 * r * s^2 + s^4)

  1 / s^4 * (r - d - num / denom)
}
