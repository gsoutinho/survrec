\name{bladder5}
\alias{bladder5}
\docType{data}
\title{bladder5
}
\description{bladder5 data set.}
\usage{data("bladder5")}
\format{
  A data frame with 85 observations on the following 10 variables.
  \describe{
    \item{\code{t1}}{First time or censoring time.}
    \item{\code{d1}}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t2}}{Second time or censoring time.}
    \item{\code{d2}}{Indicator of the second time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t3}}{Third time or censoring time.}
    \item{\code{d3}}{Indicator of the third time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t4}}{The total time of the process}
    \item{\code{d4}}{Censoring indicator of the survival time of the process; 0 if the total time is
    censored and 1 otherwise.}
    \item{\code{rx}}{Values of covariate rx.}
    \item{\code{size}}{Values of covariate size.}
  }
}

\examples{
data(bladder5)
}
\keyword{datasets}
