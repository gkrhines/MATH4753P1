
#' Identify Outliers Function
#'
#' @param vector
#'
#' @return a dataframe with two columns. First column shows us the value and the second column ("Status") identifies whether a value is "Not Outlier", "Possible", or "Outlier"
#' @export
#'
#' @examples
#' x <- c(4,90,100,95); identifyOutliers(x)
identifyOutliers <- function(vector)
{
  labels <- c()
  
  for(i in 1:length(vector))
  {
    tempZ <- (vector[i]-mean(vector))/sd(vector)
    
    if(abs(tempZ)>=2 & abs(tempZ)<=3)
    {
      labels <- append(labels, "Possible")
    }
    else if(abs(tempZ) >= 3)
    {
      labels <- append(labels, "Outlier")
    }
    else
    {
      labels <- append(labels, "Not Outlier")
    }
  }
  
  #Creating a data.frame out of our results
  output <- data.frame("Value" = vector, "Status" = labels)
  
  
  return(output)
}
