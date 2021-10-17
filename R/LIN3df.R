#' Lin's estimator for the general case of K gap times distribution function.
#' 
#' @description Provides estimates for the bivariate distribution function based on the extension the Lin's
#' estimator to the general case of K gap times.
#' @usage LIN3df(object, x, y, z)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the general case of distribution function.
#' @param y The second time for obtaining estimates for the general case of distribution function.
#' @param z The third time for obtaining estimates for the general case of distribution function.
#' @return Vector with the Lin's estimates for the general case of K gapes times distribution function.
#' @references Lin, D. Y., Sun, W. and Ying, Z. (1999). Nonparametric estimation of the gap time distributions
#' for serial events with censored data, Biometrika 86, 59-70.
#' @seealso \code{\link{LDM3df}}, \code{\link{KMW3df}} and \code{\link{WCH3df}}.
#' 
#' @examples
#' b4state <- multidf(time1=bladder5state$y1, event1=bladder5state$d1, 
#' time2= bladder5state$y1+bladder5state$y2, event2=bladder5state$d2, 
#' time=bladder5state$y1+bladder5state$y2+bladder5state$y3, status=bladder5state$d3)
#' head(b4state)[[1]]
#' 
#' LIN3df(b4state,x=13,y=20,z=40)
#' 
#' b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
#'               time2= bladder4$t2, event2=bladder4$d2,
#'               time=bladder4$t3, status=bladder4$d3)

#' LIN3df(b4,x=13,y=20,z=40)

#' @author Gustavo Soutinho and Luis Meira-Machado


LIN3df <-
function(object, x, y, z)
{
 obj <- object[[1]]
  est <- 0
  aux <- rep(0, length(obj$time1))
  for (j in 1:length(obj$time1)) { if (obj$time1[j] <= x & obj$time2[j]-obj$time1[j] <= y & obj$time[j]-obj$time2[j] > z) aux[j] <- 1/KM(obj$time, 1 - obj$status, t = obj$time1[j] +obj$time2[j] +z)}
    aux <- aux/length(obj$time1)
    est <- LINdf(object=object, x=x, y=y) - sum(aux)
return(est)                    
}
