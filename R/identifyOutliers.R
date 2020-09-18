
#' Identify Outliers Function
#'
#' @param vector
#'
#' @return a list of length two. The first element of the list comtains all values that are possibly outliers, while the second element contain all values that are definitely outliers
#' @export
#'
#' @examples
#' x <- c(4,90,100,95); identifyOutliers(x)
identityOutliers <- function(vector)
{
  output <- list()

  #Setting up the output object
  output[["Possible"]] <- c(999)
  output[["Outlier"]] <- c(999)

  for(i in 1:length(vector))
  {
    tempZ <- (vector[i]-mean(vector))/sd(vector)

    if(abs(tempZ)>=2 & abs(tempZ)<=3)
    {
      output[["Possible"]] <- append(output[["Possible"]], vector[i])
    }
    else if(abs(tempZ) >= 3)
    {
      output[["Outlier"]] <- append(output[["Outlier"]], vector[i])
    }
  }
  
  #Removing the filler 999s in the output list
  if(length(output[["Possible"]]) > 1)
  {
    output[["Possible"]] <- output[["Possible"]][-1]
  }
  else
  {
    output[["Possible"]] <- NA
  }
      
  #Removing the filler 999s in the output list
  if(length(output[["Outlier"]]) > 1)
  {
    output[["Outlier"]] <- output[["Outlier"]][-1]
  }
  else
  {
    output[["Outlier"]] <- NA
  }  

  return(output)
}
