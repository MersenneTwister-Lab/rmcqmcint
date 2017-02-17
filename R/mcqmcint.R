##' Monte-Carlo and Quasi Monte-Carlo Integration
##'
##' Compute Monte-Carlo integration and Quasi Monte-Carlo Integration with
##' Low WAFOM Digital Net.
##'
##'@name rmcqmcint-package
##'@aliases rmcqmcint-package rmcqmcint
##'@docType package
##'@import Rcpp
##'@useDynLib rmcqmcint
NULL

##' Quasi Monte-Carlo Integration with Low WAFOM Digital Net
##'
##' Compute Quasi Monte-Carlo Integration with Low WAFOM Digital Net,
##' Niederreiter-Xing and Sobol.
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' }
##'
##' integrand should receive numeric vector of length s and
##' should return numeric value.
##'
##'@param integrand integrand function.
##'@param N number of repeat.
##'@param s dimention, s should be 4 <= s <= 10.
##'@param digitalNetID 1:Niederreiter-Xing, 2:Sobol.
##'@param m F2-dimention of each element, m should be 10 <= m <= 18.
##'@param probability, should be one of 0.95, 0.99, 0.999, or 0.9999.
##'@return integrated mean value and absolute error.
##'@export
qmcint <- function(integrand,
                   N,
                   s,
                   digitalNetID = 1,
                   m = 10,
                   probability = 0.99) {
    if (s < 4 || s > 10) {
        stop("s should be an integer 4 <= s <= 10")
    }
    if (m < 10 || m > 18) {
        stop("m should be an integer 10 <= m <= 19")
    }
    return(rcppQMCIntegration(integrand, N, digitalNetID, s, m, probability))
}

##' Monte-Carlo Integration with Low WAFOM Digital Net
##'
##' Compute Monte-Carlo Integration.
##'
##' integrand should receive numeric vector of length s and
##' should return numeric value.
##'
##'@param integrand integrand function.
##'@param N number of repeat.
##'@param s dimention, s should be 4 <= s <= 10.
##'@param m use 2^m samples.
##'@param probability, should be one of 0.95, 0.99, 0.999, or 0.9999.
##'@return integrated mean value and absolute error.
##'@export
mcint <- function(integrand,
                  N,
                  s,
                  m = 10,
                  probability = 0.99) {
    return(rcppMCIntegration(integrand, N, s, m, probability))
}
