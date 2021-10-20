\name{b3state}
\alias{b3state}
\docType{data}
\title{b3state}
\description{b3state data set.}
\usage{data("b3state")}
\format{A data frame with 85 observations on the following 4 variables.
Below a brief description is given for each of these variables.
  \describe{
  \item{time1}{First time or censoring time.}
  \item{time}{The total time of the process}
  \item{event1}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
  \item{status}{Censoring indicator of the survival time of the process; 0 if the total time is censored and 1 otherwise.}
}
}
\examples{
data(b3state)
str(b3state)
}
\keyword{datasets}
