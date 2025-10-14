
#' Birthday equation
#'
#' Calculates the probability that a number of people will have the same birthday
#'
#' @param n sample size of people
#'
#' @returns The probability of people having a birthday on the same day
#' @export
#'
#' @examples
#' birthday(20:24)
birthday <- function(n) {

  1 - (exp(lchoose(365,n) + lfactorial(n) - n*log(365)))

}


