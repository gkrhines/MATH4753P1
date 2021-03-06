% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identifyOutliers.R
\name{identifyOutliers}
\alias{identifyOutliers}
\title{Identify Outliers Function}
\usage{
identifyOutliers(vector)
}
\arguments{
\item{vector}{}
}
\value{
a dataframe with two columns. First column shows us the value and the second column ("Status") identifies whether a value is "Not Outlier", "Possible", or "Outlier"
}
\description{
Identify Outliers Function
}
\examples{
x <- c(4,90,100,95); identifyOutliers(x)
}
