#' myncurve
#'
#' This function plots the density curve of a normal distribution
#' The funtion shades the region under the curve from - infinity to a given value 'a'
#' then calculates the probability and retuen it to the command line
#'
#' @param a The upper limit of the shaded area
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @returns A list consisting of mu, sigma, area
#'
#' @export
#'
#' @examples
#' myncurve(a = 2, mu = 0, sigma = 1)
myncurve = function(a, mu, sigma) {
  curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, xlim = c(mu - 3 * sigma, mu + 3 * sigma))

  # Shade the area from -inf (approximated as mu - 3 * sigma) to a
  xleft = mu - 3 * sigma
  if (a > xleft) {
    xcurve = seq(xleft, min(a, mu + 3 * sigma), length = 1000)
    ycurve = dnorm(xcurve, mean = mu, sd = sigma)
    polygon(c(xleft, xcurve, min(a, mu + 3 * sigma)), c(0, ycurve, 0), col="pink")
  }

  # Calculate the probability P(X < a)
  prob = pnorm(a, mean = mu, sd = sigma)

  # Return the list
  list(mu = mu, sigma = sigma, area = prob)
}

#testthat
library(testthat)

test_that("myncurve returns correct mu", {
  result <- myncurve(a = 2, mu = 0, sigma = 1)
  expect_equal(result$mu, 0)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(a = 2, mu = 0, sigma = 1)
  expect_equal(result$sigma, 1)
})

test_that("myncurve returns correct area", {
  result <- myncurve(a = 2, mu = 0, sigma = 1)
  expected_area <- pnorm(2, mean = 0, sd = 1)
  expect_equal(result$area, expected_area)
})
