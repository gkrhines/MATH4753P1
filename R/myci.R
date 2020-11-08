#' 95% Confidence Interval of the Mean
#'
#' @param x a single set of sample statistics
#'
#' @return a vector of length 2; the first value is the lower limit and the second its upper limit
#' @export
#'
#' @examples
#' myci(c(6,7,5,4,5,6,7,5,4,4,5,5,6,5,4,4,4,5))
myci <- function(x)
{
t=qt(0.975,length(x) - 1) #Calculating the correct t value

ci=c(mean(x)-t*sd(x)/sqrt(length(x)), mean(x)+t*sd(x)/length(x))

return(ci)
}
