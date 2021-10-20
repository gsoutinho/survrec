\name{b3size}
\alias{b3size}
\docType{data}
\title{b3size}
\description{b3size data set.}
\usage{data("b3size")}
\format{A data frame with 85 observations on the following 5 variables.
Below a brief description is given for each of these variables.
  \describe{
  \item{time1}{First time or censoring time.}
  \item{time}{The total time of the process}
  \item{event1}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
  \item{status}{Censoring indicator of the survival time of the process; 0 if the total time is censored and 1 otherwise.}
  \item{size}{Values of covariate size.}
}
}

\examples{
data(b3size)
}
\keyword{datasets}
