#' Kaplan-Meier Weighted estimator for the bivariate distribution function.
#' 
#' @description Provides estimates for the bivariate distribution function
#' based on Kaplan-Meier Weights: Kaplan-Meier Weighted estimator, KMW.
#' @usage KMWdf(object, x, y)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the bivariate distribution function.
#' @param y The second time for obtaining estimates for the bivariate distribution function.
#' @return Vector with the Kaplan-Meier weights estimates for the bivariate distribution function.
#' @references de Una-Alvarez J, Meira Machado LF (2008). "A Simple Estimator of the Bivariate
#' Distribution Function for Censored Gap Times", Statistical and Probability Letters, 78, 2440-2445.
#' Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their Application", Chapter 5.
#' Cambridge University Press.
#' @seealso \code{\link{IPCWdf}}, \code{\link{LDMdf}}, \code{\link{LINdf}} and \code{\link{WCHdf}}.
#' 
#' @examples
#' data("bladder4state")
#' b3state <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#'                    time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)
#' 
#' KMWdf(b3state,x=13,y=20)
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado

KMWdf <-
function(object, x, y)
{
 obj <- object[[1]]
 ny <- length(y)
 est <- rep(0,ny)
 G <- KMW(obj$time, obj$status)
 for (i in 1:ny){
     p <- which(obj$time1 <= x & obj$time - obj$time1 <= y[i])
     est[i] <- sum(G[p])
                }  
 return(est)                    
}
