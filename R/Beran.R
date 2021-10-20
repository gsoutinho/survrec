#' Estimation of the conditional distribution function of the response, given the
#' covariate under random censoring.
#' 
#' @description Computes the conditional survival probability P(T > y|Z = z)
#' @usage Beran(time, status, covariate, delta, x, y, kernel = "gaussian", bw, 
#' lower.tail = FALSE)
#' @param time The survival time of the process.
#' @param status Censoring indicator of the total time of the process; 0 if the 
#' total time is censored and 1 otherwise.
#' @param covariate Covariate values for obtaining estimates for the conditional probabilities.
#' @param delta Censoring indicator of the covariate.
#' @param x The first time (or covariate value) for obtaining estimates for the 
#' conditional probabilities. 
#' If missing, 0 will be used.
#' @param y The total time for obtaining estimates for the conditional 
#' probabilities.
#' @param kernel A character string specifying the desired kernel. See details 
#' below for possible options. 
#' Defaults to "gaussian" where the gaussian density kernel will be used.
#' @param bw A single numeric value to compute a kernel density bandwidth.
#' @param lower.tail logical; if FALSE (default), probabilities are 
#' P(T > y|Z = z) otherwise, P(T <= y|Z = z).
#' @details Possible options for argument window are "gaussian", "epanechnikov",
#'  "tricube", "boxcar",
#' "triangular", "quartic" or "cosine"
#' @references R. Beran. Nonparametric regression with randomly censored survival
#'  data. Technical report, 
#' University of California, Berkeley, 1981.
#' 
#' @examples
#' b3state2 <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#' time=bladder4state$y1+bladder4state$y2,
#' status=bladder4state$d2, size=bladder4state$size)
#' head(b3state2[[1]])
#' 
#' ##P(T>y|size=3)
#' library(KernSmooth)
#' obj0 <- b3state2[[1]]
#' h <- dpik(obj0$size)
#' Beran(time = obj0$time, status = obj0$status, covariate =obj0$size, x = 3, 
#' y = 50, bw = h)
#' 
#' ##P(T<=y|size=3)
#' Beran(time = obj0$time, status = obj0$status, covariate =obj0$size, x = 3,
#'  y = 50, bw = h,
#' lower.tail = TRUE)
#' @author Gustavo Soutinho and Luis Meira-Machado
#' @export Beran
#' @useDynLib survrec, .registration=TRUE

Beran <- function(
  time,
  status,
  covariate,
  delta,
  x,
  y,
  kernel="gaussian",
  bw,
  lower.tail=FALSE
) {
  spa <- NULL;
  len <- length(time);
  if ( missing(delta) ) delta <- rep(1, len);
  res <- .C(
    "SurvBeranKernel",
    as.double(time),
    as.integer(status),
    as.double(covariate),
    as.integer(delta),
    as.integer(len),
    as.double(y),
    as.double(x),
    as.double(bw),
    as.character(kernel),
    p = as.double(1),
    PACKAGE="survrec"
  )$p;
  if (lower.tail == TRUE) res <- 1 - res;
  return(res);
} # Beran
