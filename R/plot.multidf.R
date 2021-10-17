#' Plot methods for a survrec object
#' 
#' @description Provides the plots for the bivariate distribution function and marginal distribution of the second time.
#' @usage \method{plot}{multidf}(x, t1, method = "KMW", type = "s", ...)
#' @param x An object of class survrec.
#' @param t1 Value of the first gap time.
#' @param method A character string specifying which estimator to fit. Possible values are "KMW", "LIN", "WCH" 
#' and "LANDMARK".
#' @param type The type of plot that should be drawn. See details \code{\link{par}} for possible options. 
#' Defaults to "s" for the draw be stair steps.
#' @param ... Other options.
#' @return No value is returned.
#' 
#' @references de Una-Alvarez, J. and Meira-Machado, L. (2008). A simple estimator of the bivariate
#' distribution function for censored gap times, Statistics and Probability Letters 78, 2440-2445.
#'  
#' Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their Application", Chapter 5. Cambridge
#' University Press.
#'   
#' van Houwelingen, H.C. (2007). Dynamic prediction by landmarking in event history analysis, Scandinavian
#' Journal of Statistics, 34, 70-85.
#' Kaplan, E. and Meier, P. (1958). Nonparametric Estimation from Incomplete Observations, Journal of the
#' American Statistical Association 53(282), 457-481.
#' @seealso \code{\link{KMWdf}}, \code{\link{LDMdf}}, \code{\link{LINdf}} and \code{\link{WCHdf}}.
#' 
#' @examples
#' data("bladder4state")
#' b3state <- multidf(time1=bladder4state$y1, event1=bladder4state$d1,
#'                    time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)
#' 
#' head(b3state[[1]])
#' 
#' KMWdf(b3state,x=13,y=20)
#' LDMdf(b3state,x=13,y=20)
#' LINdf(b3state,x=13,y=20)
#' WCHdf(b3state,x=13,y=20)
#' 
#' plot(x=b3state, t1=3, method="KMW", type = "s")
#' plot(x=b3state, t1=3, method="LIN", type = "s")
#' plot(x=b3state, t1=3, method="WCH", type = "s")
#' plot(x=b3state, t1=3, method="LANDMARK", type = "s")
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado

plot.multidf <-
function(x, t1, method="KMW", type = "s",...)
{
if (class(x)!="multidf") stop("The argumment 'x' must be of classe 'multidf'")
ob <- x[[1]]
time1 <- ob$time1
time <- ob$time
event1 <- ob$event1
status <- ob$status

t2 <- sort(unique(c(time1, time)))
if(method == "KMW"){ auxi <- KMWdf(x, t1, t2)}
if(method == "LIN"){auxi <- LINdf(x, t1, t2)}
if(method == "WCH"){auxi <- WCHdf(x, t1, t2)} #Wang and Wells
if(method == "LANDMARK" | method == "LDM"){auxi <- LDMdf(x, t1, t2)}          
plot(auxi ~ t2, type = type,...)
#obj <- list(y = t2, t1 = t1, est = auxi)
#return(invisible(obj))
}
