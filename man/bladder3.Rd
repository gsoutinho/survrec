\name{bladder3}
\alias{bladder3}
\docType{data}
\title{bladder3
}
\description{bladder3-description
}
\usage{data("bladder3")}
\format{
  A data frame with 85 observations on the following 6 variables.
  \describe{
    \item{\code{t1}}{First time or censoring time.}
    \item{\code{d1}}{Indicator of the first time; 0 if the first time is censored and 1 otherwise.}
    \item{\code{t2}}{The total time of the process}
    \item{\code{d2}}{Censoring indicator of the survival time of the process; 0 if the total time is
    censored and 1 otherwise.}
    \item{\code{rx}}{Values of covariate rx.}
    \item{\code{size}}{Values of covariate size.}
    
  }
}
\examples{
data(bladder3)
str(bladder3) 
}
\keyword{datasets}
