##' Monte-Carlo and Quasi Monte-Carlo Integration
##'
##' Compute Monte-Carlo integration and Quasi Monte-Carlo Integration with
##' Low WAFOM Digital Net.
##'
##'@name rmcqmcint-package
##'@aliases rmcqmcint-package rmcqmcint
##'@docType package
##'@import Rcpp
##'@import RSQLite
##'@importFrom stats runif
##'@useDynLib rmcqmcint
NULL

##' get minimum and maximum dimension number of DigitalNet
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
##'@return supportd minimum and maximum dimension number for specified digitalNet.
##'@export
digitalnet.dimMinMax <- function(digitalNetID) {
  if (digitalNetID == 1) {
    netname <- "nxlw"
  } else if (digitalNetID == 2) {
    netname <- "solw"
  } else if (digitalNetID == 3) {
    return(c(2, 21201))
  } else {
    stop("invalid digitalNetID")
  }

  fmt <-
    "select min(dimr) as min, max(dimr) as max from digitalnet where netname='%s';"
  sql <- sprintf(fmt, netname)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv,
                   dbname = system.file("extdata",
                                        "digitalnet.sqlite3", package = "rmcqmcint"))
  a <- dbGetQuery(con, sql)
  dbDisconnect(con)
  c(a[1, 1], a[1, 2])
}

##' get minimum and maximum F2 dimension number of DigitalNet
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
##'@param dimR dimention.
##'@return supportd minimum and maximum F2 dimension number for specified digitalNet.
##'@export
digitalnet.dimF2MinMax <- function(digitalNetID, dimR) {
  if (digitalNetID == 1) {
    netname <- "nxlw"
  } else if (digitalNetID == 2) {
    netname <- "solw"
  } else if (digitalNetID == 3) {
    return(c(10, 31))
  } else {
    stop("invalid digitalNetID")
  }
  fmt <- paste("select min(dimf2) as min, max(dimf2) as max ",
          "from digitalnet where netname='%s' and dimr = %d;")
  sql <- sprintf(fmt, netname, dimR)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv,
                   dbname = system.file("extdata",
                                        "digitalnet.sqlite3",
                                        package = "rmcqmcint"))
  a <- dbGetQuery(con, sql)
  dbDisconnect(con)
  c(a[1, 1], a[1, 2])
}

##' get points from Digital Net
##'
##' DigitalNetID:
##' \itemize{
##' \item{1:}{Niederreiter-Xing low WAFOM}
##' \item{2:}{Sobol low WAFOM}
##' \item{3:}{Sobol Point Set up to dimension 21201}
##' }
##'
##'@param digitalNetID 1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
##'3:Sobol large dimension.
##'@param dimR dimention.
##'@param dimF2 F2-dimention of each element.
##'@param count number of points.
##'@param digitalShift use digital shift or not.
##'@return matrix of points where every row contains dimR dimensional point.
##'@export
digitalnet.points <- function(digitalNetID,
                              dimR,
                              dimF2 = 10,
                              count,
                              digitalShift = FALSE) {
  if (digitalNetID != 1 && digitalNetID != 2 && digitalNetID != 3) {
    stop("digitalNetID should be 1 or 2 or 3.")
  }
  if (digitalNetID == 1) {
    netname <- "nxlw"
  } else if (digitalNetID == 2) {
    netname <- "solw"
  }
  smax = digitalnet.dimMinMax(digitalNetID)
  if (dimR < smax[1] || dimR > smax[2]) {
    stop(sprintf("dimR should be an integer %d <= dimR <= %d", smax[1], smax[2]))
  }
  if (missing(dimF2)) {
    dimF2 = max(dimF2, ceiling(log2(count)))
  }
  mmax = digitalnet.dimF2MinMax(digitalNetID, dimR)
  if (dimF2 < mmax[1] || dimF2 > mmax[2]) {
    stop(sprintf("dimD2 should be an integer %d <= dimF2 <= %d", mmax[1], mmax[2]))
  }
  if (digitalNetID == 3) {
    fmt <- paste("select d, s, a, mi ",
                 "from sobolbase where s <= %d ",
                 "order by d asc;")
    sql <- sprintf(fmt, dimR)
  } else {
    fmt <- paste("select dimr, dimf2, wafom, tvalue, data from digitalnet ",
                 "where netname='%s' and dimr = %d and dimf2 = %d;")
    sql <- sprintf(fmt, netname, dimR, dimF2)
  }
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv,
                   dbname = system.file("extdata",
                                        "digitalnet.sqlite3",
                                        package = "rmcqmcint"))
  df <- dbGetQuery(con, sql)
  dbDisconnect(con)
  if (digitalShift) {
    sv <- runif(2*dimR, min=-2^31, max=2^31-1)
  } else {
    sv <- numeric(1)
  }
#  print(sv)
  return(rcppDigitalNetPoints(df, digitalNetID, dimR, dimF2, count, sv))
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
  if (digitalNetID != 1 && digitalNetID != 2 && digitalNetID != 3) {
    stop("digitalNetID should be 1 or 2 or 3.")
  }
  dimr = digitalnet.dimMinMax(digitalNetID)
  if (s < dimr[1] || s > dimr[2]) {
    stop(sprintf("s should be an integer %d <= s <= %d", dimr[1], dimr[2]))
  }
  dimf2 = digitalnet.dimF2MinMax(digitalNetID, s)
  if (m < dimf2[1] || m > dimf2[2]) {
    stop(sprintf("m should be an integer %d <= m <= %d", dimf2[1], dimf2[2]))
  }
  if (digitalNetID == 3) {
    fmt <- paste("select d, s, a, mi ",
                 "from sobolbase where s <= %d ",
                 "order by d asc;")
    sql <- sprintf(fmt, s)
  } else {
    if (digitalNetID == 1) {
      netname <- "nxlw"
    } else if (digitalNetID == 2) {
      netname <- "solw"
    }
    fmt <- paste("select dimr, dimf2, wafom, tvalue, data from digitalnet ",
                 "where netname='%s' and dimr = %d and dimf2 = %d;")
    sql <- sprintf(fmt, netname, s, m)
  }
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv,
                   dbname = system.file("extdata",
                                        "digitalnet.sqlite3",
                                        package = "rmcqmcint"))
  df <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(rcppQMCIntegration(integrand, N, df, digitalNetID, s, m, probability))
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
