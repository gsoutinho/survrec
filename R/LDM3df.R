#' Landmark estimator for the general case of K gap times distribution function.
#' 
#' @description Provides estimates for the general case of K gap times distribution function based on landmarking.
#' @usage LDM3df(object, x, y, z)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the general case of distribution function.
#' @param y The second time for obtaining estimates for the general case of distribution function.
#' @param z The third time for obtaining estimates for the general case of distribution function.
#' 
#' @references van Houwelingen, H.C. (2007). Dynamic prediction by landmarking in event history analysis,
#' Scandinavian Journal of Statistics, 34, 70-85.
#' Kaplan, E. and Meier, P. (1958). Nonparametric Estimation from Incomplete Observations, Journal of the
#' American Statistical Association 53(282), 457-481. 
#' @seealso \code{\link{LDM3df}}, \code{\link{LIN3df}} and \code{\link{WCH3df}}.
#' 
#' @examples
#' b4state <- multidf(time1=bladder5state$y1, event1=bladder5state$d1,
#' time2= bladder5state$y1+bladder5state$y2, event2=bladder5state$d2,
#' time=bladder5state$y1+bladder5state$y2+bladder5state$y3,
#' status=bladder5state$d3)
#' head(b4state)[[1]]
#' 
#' LDM3df(b4state,x=13,y=20,z=40)
#' 
#' b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
#'              time2= bladder4$t2, event2=bladder4$d2,
#'              time=bladder4$t3, status=bladder4$d3)

#'LDM3df(b4,x=13,y=20,z=40)
#'
#' @author Gustavo Soutinho and Luis Meira-Machado

LDM3df <-
function(object, x, y, z)
{
 obj <- object[[1]]
 est <- 0
 p1<- which(obj$time1<=x & obj$time2 - obj$time1<=y)
 G0 <- KMW(obj$time2, obj$event2)
 aux <- sum(G0[p1])
 
 time1_2 <- obj$time1[p1]
 time2_2 <- obj$time2[p1] - obj$time1[p1]
 time3_2 <- obj$time[p1] - obj$time2[p1] 
 Stime3_2 <- obj$time[p1]
 event1_2 <- obj$event1[p1]
 event2_2 <- obj$event2[p1]
 event3_2 <- obj$status[p1]
  G <- KMW(Stime3_2, event3_2)
  p2 <- which(time3_2<=z)
  est <- sum(G[p2])*aux
 return(est)                    
}
