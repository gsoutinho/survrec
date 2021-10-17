#' Create a multidf object
#' 
#' @description Creates a "multidf" object, usually used as a response variable in a model formula.
#' @usage multidf(time1, time, event1, status, ...)
#' @param time1 First time or censoring time.
#' @param time The total time of the process.
#' @param event1 Indicator of the first time; 0 if the first time is censored and 1 otherwise.
#' @param status Censoring indicator of the survival time of the process; 0 if the total time is
#' censored and 1 otherwise.
#' @param ... Other options. For instance, for three gap times: \code{time2} represent the second
#' time and \code{event2} the censoring indicator of the second time; 0 if the second time is censored and 1 otherwise. Other additional arguments, such as covariates, can also be included in the data set.
#' @return An object of class "multidf". "multidf" objects are implemented as a single data frame.

#' @details Arguments in this function must be introduced in the following
#' order: \code{time1}, \code{event1}, \code{time} and \code{status}, where
#' \code{time1} and \code{time} are ordered event times and
#' \code{event1} and \code{event} their corresponding indicator statuses.
#' Other arguments can be also added. These should consider intermediate times and corresponding
#' censoring indicators or covariates.
#' @examples
#' data("bladder4state")
#' b3state <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
#'                   time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)

#'head(b3state[[1]])
#'data("bladder4")
#'
#'b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
#'              time2= bladder4$t2, event2=bladder4$d2,
#'              time=bladder4$t3, status=bladder4$d3)

#'head(b4state[[1]])
#'
#' @author Gustavo Soutinho and Luis Meira-Machado
#' 
#' @importFrom "KernSmooth" dpik
#' @importFrom "survidm" tprob KM KMW NWW Beran
#' @importFrom "survival" coxph Surv survfit strata untangle.specials 
#' @importFrom "graphics" legend abline axis legend lines matplot par plot polygon
#' @importFrom "stats" pchisq pnorm quantile sd na.omit terms approxfun as.formula rnorm rpois weighted.mean
#' @importFrom "utils" capture.output
#' @importFrom "stats" model.matrix model.frame model.response model.offset  
#' @importFrom "stats" delete.response delete.response

#' @export Beran
#' @export IPCWdf
#' @export KMW3df
#' @export KMWdf
#' @export LDM3df
#' @export LDMdf
#' @export LIN3df
#' @export LINdf
#' @export multidf
#' @export NWW
#' @export plot.multidf
#' @export WCH3df
#' @export WCHdf
#' @S3method plot multidf

multidf <-
function (time1, time, event1, status, ...)
{
    if (missing(time1))
        stop("Argument 'time1' is missing, with no default")
    if (missing(event1))
        stop("Argument 'event1' is missing, with no default")
    if (missing(time))
        stop("Argument 'time' is missing, with no default")
    if (missing(status))
        stop("Argument 'status' is missing, with no default")

    data <- list(time1 = as.double(time1), time = as.double(time), event1 = as.integer(event1), status = as.integer(status), ...)
    datalen <- length(data)
    if (datalen > 4) {
        datanames <- names(data)
        for (i in 5:datalen) {
            if (!is.numeric(data[[i]]))
                stop("All additional arguments must be numeric")
            if (length(data[[i]]) != length(time1))
                stop("All additional arguments must have the same length as arguments 'time1', 'time', 'event1', and 'status'")
            if (datanames[i] == "")
                datanames[i] <- paste("covariate", i - 4, sep = ".")
            if (!is.double(data[[i]]))
                data[[i]] <- as.double(data[[i]])
        }
        names(data) <- datanames
    }
    attr(data, "row.names") <- as.integer(1:length(time1))
    attr(data, "class") <- "data.frame"
    object <- vector(mode = "list", length = 1)
    object[[1]] <- na.omit(data)
    attr(object, "class") <- "multidf"
    return(object)
}
