#' Kaplan-Meier product-limit estimate of survival
#' 
#' @description This function provides survival estimates using the product-limit Kaplan-Meier estimator.
#' 
#' @usage KM(time, status, t)
#' 
#' @param time Survival time of the process.
#' @param  status Censoring indicator of the survival time of the process; 0 if the survival time is 
#' censored and 1 otherwise.
#' @param t The time for obtaining survival estimates.
#' 
#' @references E. Kaplan and P. Meier. Nonparametric estimation from incomplete observations. Journal of
#' the American Statistical Association, 53:457-481, 1958.
#' @examples
#' require(survival)
#' data("bladder4state")
#' obj<- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#'               time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)
#' obj2<-obj[[1]]
#' KM(time = obj2$time, status = obj2$status, t = 20)

#' fit <- survfit(Surv(obj2$time, obj2$status) ~ 1, data = obj2)
#' summary(fit, time = 20)$surv
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado
#' @export KM

KM <- function(
  time,
  status,
  t
) {
  if (missing(time)) stop("Argument 'time' is missing with no default");
  if (missing(status)) stop("Argument 'status' is missing with no default");
  if (missing(t)) stop("Argument 't' is missing with no default");
  return(
    .C(
      "KaplanMeierValueSort", as.double(time), as.integer(status),
      as.integer( length(time) ), as.double(t), p=as.double(1),
      PACKAGE="survrec"
    )$p
  );
} # KM
