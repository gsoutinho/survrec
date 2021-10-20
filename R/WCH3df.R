#' Weighted cumulative hazard estimator for the general case of K gap times distribution function.
#' 
#' @description Provides estimates for the general case of K gap times distribution function based based on
#' Weighted cumulative hazard estimator, WCH.
#' @usage WCH3df(object, x, y, z)
#' @param object An object of class multidf. 
#' @param x The first time for obtaining estimates for the general case of K gap times distribution function.
#' @param y The second time for obtaining estimates for the general case of K gap times distribution function.
#' @param z The third time for obtaining estimates for the general case of K gap times distribution function.
#' @return Vector with the Weighted cumulative hazard estimates for the general case of K gap times
#' distribution function.
#' 
#' @references Wang, M.C. and Wells, M.T. (1998). Nonparametric Estimation of successive duration
#' times under dependent censoring, Biometrika 85, 561-572.
#' @seealso \code{\link{KMW3df}}, \code{\link{LIN3df}} and \code{\link{LDM3df}}.
#' 
#' @examples
#' b4state <- multidf(time1=bladder5state$y1, event1=bladder5state$d1, 
#' time2= bladder5state$y1+bladder5state$y2, event2=bladder5state$d2,
#' time=bladder5state$y1+bladder5state$y2+bladder5state$y3, status=bladder5state$d3)
#' 
#' head(b4state)[[1]]
#' 
#' WCH3df(b4state,x=13,y=20,z=40)
#' 
#' b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
#'               time2= bladder4$t2, event2=bladder4$d2,
#'               time=bladder4$t3, status=bladder4$d3)
#'               
#' WCH3df(b4,x=13,y=20,z=40)
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado

WCH3df <-
function(object, x, y, z)
{
 obj <- object[[1]]
 time1 <- obj$time1
 time2 <- obj$time2 - obj$time1
 time3 <- obj$time - obj$time2 
 time <- obj$time
 event1 <- obj$event1
 event2 <- obj$event2
 status <- obj$status

  Stime <- time[status == 0]
  Stime <- c(0, Stime, max(time))
  Stime <- sort(unique(Stime))
  cen.surv <- sapply(Stime, function (u) KM(time, 1 - status, u))	
  
  w <- sort(unique(time3[time3 <= z & time3 > 0]))
  nw <- length(w)
  dv <- rep(0, nw)
  dv2 <- rep(0, nw)
  
  pn <- lapply(w, function (u) which(time1 <= x & time2 <= y & status == 1 & time3 == u))
  pd <- lapply(w, function (u) which(time1 <= x & time2 <= y & event2 == 1 & time3 >= u))
  
  if (z == 0) res <- 0
  else {
  for (i in 1:nw) {
    m1 <- length(pn[[i]])
    m2 <- length(pd[[i]])
    if (m1 == 0) Gn <- 0
    else {
      Gn <- vector(length = m1)
      for (j in 1:m1) { p0 <- max(which(Stime <= time1[pn[[i]]][j] + time2[pn[[i]]][j] + w[i]))
      Gn[j] <- 1/cen.surv[p0]}}
    
    if (m2 == 0) Gd <- 1
    else {
      Gd <- vector(length = m2)
      for (j in 1:m2) { p0 <- max(which(Stime <= time1[pd[[i]]][j] + time2[pd[[i]]][j] + w[i]))
      Gd[j] <- 1/cen.surv[p0]}}
    
    dv[i] <- ifelse(sum(Gd) == 0, 1 , 1 - sum(Gn) / sum(Gd))
  }
  res <- (1 - prod(dv)) * WCHdf(object, x, y)
  }
  
  
  if (res < 0) res <- 0
  if (res > 1) res <- 1
  return(res)
}
