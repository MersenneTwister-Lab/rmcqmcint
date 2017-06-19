##' Monte-Carlo and Quasi Monte-Carlo Integration
##'
##' Compute Monte-Carlo integration and Quasi Monte-Carlo Integration with
##' Low WAFOM Digital Net.
##'
##'@name rmcqmcint-package
##'@aliases rmcqmcint-package rmcqmcint
##'@docType package
##'@import Rcpp, RSQLite
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
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##' integrand should receive numeric vector of length s and
##' should return numeric value.
##'
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
##'@return supportd minimal and maximal dimension number for specified digitalNet.
##'@export
digitalnet.dimMinMax <- function(digitalNetID) {
  if (digitalNetID == 1) {
    netname <- "nxlw"
  } else if (digitalNetID == 2) {
    netname <- "solw"
  } else if (digitalNetID == 3) {
    return c(2, 21201)
  } else {
    stop("invalid digitalNetID")
  }
    fmt <- "select min(dimr) as min, max(dimr) as max from digitalnet where netname='%s';"
    sql <- sprintf(fmt, netname)
    drv<-dbDriver("SQLite")
    con<-dbConnect(drv,dbname=system.file("extdata",
    "digitalnet.sqlite3", package="rmcqmcint"))
    a<-dbGetQuery(con, sql)
    c(a[1,1], a[1,2])
}

##' Quasi Monte-Carlo Integration with Low WAFOM Digital Net
##'
##' Compute Quasi Monte-Carlo Integration with Low WAFOM Digital Net,
##' Niederreiter-Xing and Sobol.
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##' integrand should receive numeric vector of length s and
##' should return numeric value.
##'
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
##'@return supportd minimal and maximal dimension number for specified digitalNet.
##'@export
digitalnet.dimF2MinMax <- function(digitalNetID, dimR) {
  if (digitalNetID == 1) {
    netname <- "nxlw"
  } else if (digitalNetID == 2) {
    netname <- "solw"
  } else if (digitalNetID == 3) {
    return c(10, 31)
  } else {
    stop("invalid digitalNetID")
  }
  fmt <- "select min(dimf2) as min, max(dimf2) as max from digitalnet where netname='%s' and dimr = %d;"
  sql <- sprintf(fmt, netname, dimR)
  drv<-dbDriver("SQLite")
  con<-dbConnect(drv,dbname=system.file("extdata",
                                        "digitalnet.sqlite3", package="rmcqmcint"))
  a<-dbGetQuery(con, sql)
  c(a[1,1], a[1,2])
}

##' Quasi Monte-Carlo Integration with Low WAFOM Digital Net
##'
##' Compute Quasi Monte-Carlo Integration with Low WAFOM Digital Net,
##' Niederreiter-Xing and Sobol.
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##' integrand should receive numeric vector of length s and
##' should return numeric value.
##'
##'@param integrand integrand function.
##'@param N number of repeat.
##'@param s dimention, s should be 4 <= s
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
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
    if (digitalNetID == 3) {
        if (s < 2 || s > 21201) {
            stop("s should be an integer 2 <= s <= 21201")
        }
    } else {
        if (s < 4 || s > 10) {
            stop("s should be an integer 4 <= s <= 10")
        }
    }
    if (m < 10 || m > 18) {
        stop("m should be an integer 10 <= m <= 18")
    }
    if (digitalNetID == 3) {
       return(rcppQMCIntegrationSobol(integrand, N, sobolbase, s, m, probability))
    } else {
       return(rcppQMCIntegration(integrand, N, digitalNetID, s, m, probability))
    }
}

##' Monte-Carlo Integration
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
