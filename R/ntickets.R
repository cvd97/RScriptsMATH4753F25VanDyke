#' ntickets function
#'
#' Find optimal number of tickets to sell for a flight
#'
#' Calculates the optimal number of tickets to sell when a flight has N seats,
#' the probability a passenger shows up is p, and gamma is the acceptable
#' probability of a truly overbooked flight (more passengers show than seats)
#'
#' @param N Number of seats in the aircraft
#' @param gamma Target probability of an overbook event
#' @param p Probability that any given passenger shows up
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline labs theme_minimal
#' @importFrom stats pbinom uniroot
#'
#' @return Named list containing nd, nc, N, p and gamma
#' where nd is calculated using the discrete distribution and nc is the same calculated with normal approximation
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  q <- 1 - p

  # Normal approximation for nc
  obj_norm <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * q)
    1 - pnorm((N + 0.5 - mu) / sigma) - gamma
  }

  # Find suitable interval for uniroot
  lower <- N
  upper <- ceiling(N / p) + 10
  while (obj_norm(lower) > 0 && lower > 0) lower <- lower - 10
  while (obj_norm(upper) < 0) upper <- upper + 10

  nc <- uniroot(obj_norm, c(lower, upper))$root

  # Discrete for nd
  n_start <- N
  n_end <- ceiling(N / p) + 10
  nvals <- seq(n_start, n_end, by = 1)
  p_over <- 1 - pbinom(N, nvals, p)
  obj_disc <- p_over - gamma

  # Find n where abs(obj_disc) is minimized
  ind <- which.min(abs(obj_disc))
  nd <- nvals[ind]

  # Print the named list
  res <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(res)

  # Plot for discrete using ggplot2
  df_disc <- data.frame(n = nvals, obj = obj_disc)
  print(ggplot(df_disc, aes(x = n, y = obj)) +
          geom_point(color = "black") +
          geom_line() +
          geom_hline(yintercept = 0, color = "red") +
          geom_point(data = df_disc[ind, ], aes(x = n, y = obj), color = "red", size = 4) +
          labs(title = sprintf("Objective Vs n to find optimal tickets sold\n  (%d), N = %d, gamma = %.2f, p = %.2f Discrete",nd, N, gamma, p), x = "n", y = "Objective") +
          theme_minimal())

  # Plot for continuous using ggplot2
  ncont <- seq(n_start, n_end, length.out = 300)
  obj_cont <- sapply(ncont, obj_norm)
  df_cont <- data.frame(n = ncont, obj = obj_cont)
  print(ggplot(df_cont, aes(x = n, y = obj)) +
          geom_line(color = "black") +
          geom_hline(yintercept = 0, color = "red") +
          labs(title = sprintf("Objective Vs n to find optimal tickets sold \n (%.4f), N = %d, gamma = %.2f, p = %.2f Continuous", nc, N, gamma, p), x = "n", y = "Objective") +
          theme_minimal())
}
