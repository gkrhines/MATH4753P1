
#' A Normal Curve with Lower Tail Area
#'
#' @param upper the upper bound of the area
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return A curve, shaded region, and area
#' @export
#'
#' @examples
#' myncurve(upper = 4, mu = 4, sd = 5)
myncurve = function(upper, mu, sigma)
{
  #Plotting the specified curve
  curve(dnorm(x,mean=mu,sd=sigma),xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,upper,length=1000) #Getting x-values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma) #Getting y-values
  polygon(c(mu-3*sigma,xcurve,upper),c(0,ycurve,0),col="Red") #filling in the created shape

  #Getting area and putting it in a list
  area = round(pnorm(upper,mean=mu,sd=sigma),4)

  output <- list()
  output["area"] <- area

  return(output)
}
