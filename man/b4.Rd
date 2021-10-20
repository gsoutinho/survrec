\name{b4}
\alias{b4}
\docType{data}
\title{b4}
\description{b4 data set.}
\usage{data("b4")}
\format{A data frame with 85 observations on the following 6 variables.
Below a brief description is given for each of these variables.
  \describe{
  \item{time1}{First time or censoring time.}
  \item{time2}{Second time.}
  \item{time}{The total time of the process}
  \item{event1}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
  \item{event2}{Indicator of the second time; 0 if the first time is censored and 1 otherwise.}
  \item{status}{Censoring indicator of the survival time of the process; 0 if the total time is censored and 1 otherwise.}
}
}
\examples{
data(b4)
head(b4)
}
\keyword{datasets}
