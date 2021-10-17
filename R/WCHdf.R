#' Weighted cumulative hazard estimator for the bivariate distribution function
#' 
#' @description Provides estimates for the bivariate distribution function based on Weighted
#' cumulative hazard estimator, WCH.
#' @usage WCHdf(object, x, y)
#' @param object An object of class multidf. 
#' @param x The first time for obtaining estimates for the bivariate distribution function.
#' @param y The second time for obtaining estimates for the bivariate distribution function.

#' @return Vector with the Weighted cumulative hazard estimates for the bivariate distribution function.
#' 
#' @references Wang, M.C. and Wells, M.T. (1998). Nonparametric Estimation of successive duration
#' times under dependent censoring, Biometrika 85, 561-572.
#' @seealso \code{\link{IPCWdf}}, \code{\link{KMWdf}}, \code{\link{LINdf}} and \code{\link{LDMdf}}.
#' 
#' @examples
#' data("bladder3")
#' 
#' b3 <- multidf(time1=bladder3$t1, event1=bladder3$d1, 
#'               time=bladder3$t2, status=bladder3$d2)

#' head(b3[[1]])

#' WCHdf(b3,x=13,y=20)
#'
#' @author Gustavo Soutinho and Luis Meira-Machado

WCHdf <-
function(object, x, y)
{
 obj <- object[[1]]
 time1 <- obj$time1
 time <- obj$time
 time2 <- time - time1
 event1 <- obj$event1
 status <- obj$status
 ny <- length(y)
 est <- rep(0, ny)
 aux <- rep(0, length(time1))
 p0 <- which(time1 <= x) 
 G0 <- KMW(time1, event1)
 f0 <- sum(G0[p0])
 print("This may take a few minutes...")
 for (k in 1:ny){
      w <- sort(unique(time2[time2 <= y[k]]))
      nw <- length(w)
      dv <- rep(0,nw)
      pn <- vector(mode = "list",length = nw)
      pd <- vector(mode = "list",length = nw)
      for (i in 1:nw) {
          pn[[i]] <- which(time1 <= x & status == 1 & time2 == w[i])
          pd[[i]] <- which(time1 <= x & event1 == 1 & time2 >= w[i])
          m1 <- length(pn[[i]])
          m2 <- length(pd[[i]])
          if (m1 == 0) Gn <- 0
          else {
          Gn <- vector(length = m1)
          for (j in 1:m1) Gn[j] <- 1/KM(time, 1 - status, t = (time1[pn[[i]]][j]+w[i]))}
          if (m2==0) Gd <- 1
          else {
          Gd <- vector(length=m2)
          for (j in 1:m2) Gd[j] <- 1/KM(time, 1 - status, t = (time1[pd[[i]]][j]+w[i]))}
          dv[i] <- ifelse(sum(Gd) == 0, 1 ,1 - sum(Gn)/sum(Gd))
                      }
      est[k] <- (1 - prod(dv)) * f0
      if (est[k] < 0) est[k] <- 0
      if (est[k] > 1) est[k] <- 1
                } 
 return(est)                    
}
