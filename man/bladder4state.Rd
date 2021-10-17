\name{bladder4state}
\alias{bladder4state}
\docType{data}
\title{bladder4state}
\description{bladder4state data set.}
\usage{data("bladder4state")}
\format{
  A data frame with 85 observations on the following 9 variables.
  \describe{
    \item{\code{id}}{Identification number}
    \item{\code{y1}}{First gap time.}
    \item{\code{d1}}{Indicator of the first gap time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{y2}}{Second gap time}
    \item{\code{d2}}{Censoring indicator of the second gap time; 0 if the total time is
    censored and 1 otherwise.}
    \item{\code{y3}}{Third gap time}
    \item{\code{d3}}{Censoring indicator of the third gap time; 0 if the total time is
    censored and 1 otherwise.}
    \item{\code{rx}}{Values of covariate rx.}
    \item{\code{size}}{Values of covariate size.}
    
    
  }
}
\examples{
data(bladder4state)
}
\keyword{datasets}
