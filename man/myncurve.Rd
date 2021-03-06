% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{A Normal Curve with Lower Tail Area}
\usage{
myncurve(upper, mu, sigma)
}
\arguments{
\item{upper}{the upper bound of the area}

\item{mu}{mean}

\item{sigma}{standard deviation}
}
\value{
A curve, shaded region, and area
}
\description{
A Normal Curve with Lower Tail Area
}
\examples{
myncurve(upper = 4, mu = 4, sd = 5)
}
