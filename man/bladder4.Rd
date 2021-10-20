\name{bladder4}
\alias{bladder4}
\docType{data}
\title{bladder4}
\description{bladder4 data set.}
\usage{data("bladder4")}
\format{
  A data frame with 85 observations on the following 8 variables.
  \describe{
    \item{\code{t1}}{First time or censoring time.}
    \item{\code{d1}}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t2}}{Second time or censoring time.}
    \item{\code{d2}}{Indicator of the second time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t3}}{The total time of the process}
    \item{\code{d3}}{Censoring indicator of the survival time of the process; 0 if the total time is
    censored and 1 otherwise.}
    \item{\code{rx}}{Values of covariate rx.}
    \item{\code{size}}{Values of covariate size.}
}
}
\examples{
data(bladder4)
}
\keyword{datasets}
