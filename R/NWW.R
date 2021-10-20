#' Nadaraya-Watson weights
#' 
#' @description Computes the Nadaraya-Watson weights.
#' @usage NWW(covariate, x, kernel = "gaussian", bw)
#' @param covariate Covariate values for obtaining weights.
#' @param x Covariate value to compute the weight at.
#' @param kernel A character string specifying the desired kernel. See details below for possible options. 
#' Defaults to "gaussian" where the gaussian density kernel will be used.
#' @param bw A single numeric value to compute a kernel density bandwidth.

#' @details Possible options for argument window are "gaussian", "epanechnikov", "tricube", "boxcar",
#' "triangular", "quartic" or "cosine". 
#' @return A vector with Nadaraya-Watson weights.
#' @examples
#' b3state2 <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#' time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2, size=bladder4state$size)
#' obj0 <- b3state2[[1]]
#' NWW(covariate = obj0$size, x=3, kernel = "gaussian", bw = 3)
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado


NWW <- function(
  covariate,
  x,
  kernel="gaussian",
  bw
) {
  len <- length(covariate);
  listg <- .C("NWWeightsKernel", as.double(covariate), as.integer(len),
    as.double(x), as.double(bw), as.character(kernel),
    weight = double(len), PACKAGE="survrec");
  return(listg$weight);
} # NWW
