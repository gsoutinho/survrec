#' Kaplan-Meier weights
#' 
#' @description This function returns a vector with the Kaplan-Meier weights.
#' 
#' @usage KMW(time, status)
#' 
#' @param time Survival time of the process.
#' @param status Censoring indicator of the survival time of the process; 0 if the survival time is censored and 
#' 1 otherwise.
#' @return Vector with Kaplan-Meier weights.
#' @references E. Kaplan and P. Meier. Nonparametric estimation from incomplete observations. Journal of the 
#' American Statistical Association, 53:457-481, 1958.
#' @examples
#' data("bladder4state")
#' obj<- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#'              time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)

#' obj2<-obj[[1]]

#' kmw <- KMW(time = obj2$time, status = obj2$status)
#' require(survival)
#' bladder.surv <- survfit(Surv(time, status) ~ 1, obj2)
#' times <- summary(bladder.surv)$time
#' surv <- summary(bladder.surv)$surv
#' nevent <- summary(bladder.surv)$n.event
#' p <- match(obj2$time, times)
#' kmw2 <- -diff(c(1, surv))/nevent
#' kmw2 <- kmw2[p]*obj2$status
#' kmw2[is.na(kmw2)] <- 0
#' all.equal(kmw, kmw2)
#' @author Gustavo Soutinho and Luis Meira-Machado

KMW <- function(
  time,
  status
) {
  t1 <- max(time);
  len <- length(time);
  res <- .C("WeightsKaplanMeierSort", time = as.double(time),
    status = as.integer(status), as.integer(len), as.double(t1),
    weights = double(len), PACKAGE="survrec"
  );
  return(res$weights);
}
