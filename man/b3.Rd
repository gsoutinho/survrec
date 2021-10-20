\name{b3}
\alias{b3}
\docType{data}
\title{b3}
\description{b3 data set.}
\usage{data("b3")}

\format{A data frame with 85 observations on the following 4 variables.
Below a brief description is given for each of these variables.
  \describe{
  \item{time1}{First time or censoring time.}
  \item{time}{The total time of the process.}
  \item{event1}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
  \item{status}{Censoring indicator of the survival time of the process; 0 if the total time is censored and 1 otherwise.}
}
}

\examples{
data(b3)
head(b3)
}
\keyword{datasets}
