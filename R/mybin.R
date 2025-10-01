#' Binomial simulation
#'
#' Runs a binomial simulation with iter independent experiments,
#' each consisting of n Bernoulli trials with success probability p.
#' Returns the relative frequencies of the number of successes and optionally
#' displays a barplot of these proportions.
#'
#' @param iter the number of iterations
#' @param n the sample size
#' @param p the probability of success for each trial
#'
#' @importFrom graphics barplot
#'
#' @return A numeric vector of proportions for 0 through n successes.Names of the vector correspond to the number of successes.
#' @export
#'
#' @examples
#' mybin(iter = 100, n = 10, p = 0.7)

mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
