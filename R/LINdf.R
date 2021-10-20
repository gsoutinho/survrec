#' Lin's estimator for the bivariate distribution function.
#' 
#' @description Provides estimates for the bivariate distribution function based on the extension the
#' Kaplan-Meier estimator of the distribution function for the first event time and the Inverse Probability of Censoring Weights for the second time.
#' @usage LINdf(object, x, y)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the bivariate distribution function.
#' @param y The second time for obtaining estimates for the bivariate distribution function 
#' @return Vector with the Lin's estimates for the bivariate distribution function.
#' @references Lin, D. Y., Sun, W. and Ying, Z. (1999). Nonparametric estimation of the gap
#' time distributions for serial events with censored data, Biometrika 86, 59-70.
#' @seealso \code{\link{IPCWdf}}, \code{\link{LDMdf}}, \code{\link{KMWdf}} and \code{\link{WCHdf}}.
#' @examples
#' data("bladder3")
#' b3 <- multidf(time1=bladder3$t1, event1=bladder3$d1, 
#'              time=bladder3$t2, status=bladder3$d2)
#'              head(b3[[1]])
#'              
#' LINdf(b3,x=13,y=20)
#' @author Gustavo Soutinho and Luis Meira-Machado

LINdf <-
function(object, x, y)
{
 obj <- object[[1]]
 ny <- length(y)
 est <- rep(0,ny)
 G <- KMW(obj$time1, obj$event1)
 p <- which(obj$time1 <= x)
 aux1 <- sum(G[p])
 aux <- rep(0,length(obj$time1))
 time2 <- obj$time - obj$time1
 for (i in 1:ny){
     for (j in 1:length(obj$time1)) { if (obj$time1[j] <= x & time2[j] > y[i]) aux[j] <- 1/KM(obj$time, 1 - obj$status, t = obj$time1[j]+y[i])}
     aux <- aux/length(obj$time1)
     est[i] <- aux1 - sum(aux)
                } 
 return(est)                    
}
