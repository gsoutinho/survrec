#' Landmark estimator for the bivariate distribution function
#' 
#' @description Provides estimates for the bivariate distribution function based on Bayes' 
#' theorem and Kaplan-Meier survival function. This approach is also named as landmarking.
#' @usage LDMdf(object, x, y)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the bivariate distribution function.
#' @param y The second time for obtaining estimates for the bivariate distribution function.
#' 
#' @references van Houwelingen, H.C. (2007). Dynamic prediction by landmarking in event history
#' analysis, Scandinavian Journal of Statistics, 34, 70-85.
#' 
#' Kaplan, E. and Meier, P. (1958). Nonparametric Estimation from Incomplete Observations, Journal
#' of the American Statistical Association 53(282), 457-481.
#' @seealso \code{\link{IPCWdf}}, \code{\link{KMWdf}}, \code{\link{LINdf}} and \code{\link{WCHdf}}.
#' 
#' @examples
#' data("bladder4state")
#' b3state <- multidf(time1=bladder4state$y1, event1=bladder4state$d1,
#'                   time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)
#'                
#' LDMdf(b3state,x=13,y=20)
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado

LDMdf <-
function(object, x, y)
{
  obj <- object[[1]]
  ny <- length(y)
  est <- rep(0,ny)
  p0 <- which(obj$time1 <= x)
  time2 <- obj$time - obj$time1
  ntime2 <- time2[p0] 
   for (i in 1:ny){
      p1 <- which(ntime2 <= y[i])
      G <- KMW(obj$time[p0], obj$status[p0]) 
      est[i] <- (1 - KM(obj$time1, obj$event1, t = x)) * sum(G[p1])
                  } 
  return(est)
}
