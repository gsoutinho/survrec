% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KMW3df.R
\name{KMW3df}
\alias{KMW3df}
\title{Kaplan-Meier Weighted estimator for the general case of K gap times distribution function.}
\usage{
KMW3df(object, x, y, z)
}
\arguments{
\item{object}{An object of class multidf.}

\item{x}{The first time for obtaining estimates for the general case of distribution function.}

\item{y}{The second time for obtaining estimates for the general case of distribution function.}

\item{z}{The third time for obtaining estimates for the general case of distribution function.}
}
\value{
Vector with the Kaplan-Meier Weighted estimates for the general case of K gapes times
distribution function.
}
\description{
Provides estimates for the general case of K gap times distribution function
based on Kaplan-Meier Weights: Kaplan-Meier Weighted estimator, KMW.
}
\examples{
b4state <- multidf(time1=bladder5state$y1, event1=bladder5state$d1,
time2= bladder5state$y1+bladder5state$y2, event2=bladder5state$d2,
time=bladder5state$y1+bladder5state$y2+bladder5state$y3, status=bladder5state$d3)
head(b4state)[[1]]

KMW3df(b4state,x=13,y=20,z=40)

b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
             time2= bladder4$t2, event2=bladder4$d2,
             time=bladder4$t3, status=bladder4$d3)
KMW3df(b4,x=13,y=20,z=40)

}
\references{
de Una-Alvarez J, Meira Machado LF (2008). "A Simple Estimator of the Bivariate Distribution
Function for Censored Gap Times", Statistical and Probability Letters, 78, 2440-2445.
Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their Application", Chapter 5.
Cambridge University Press.
}
\seealso{
\code{\link{LDM3df}}, \code{\link{LIN3df}} and \code{\link{WCH3df}}.
}
\author{
Gustavo Soutinho and Luis Meira-Machado
}
