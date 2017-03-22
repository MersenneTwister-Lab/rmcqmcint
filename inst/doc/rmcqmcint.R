## ------------------------------------------------------------------------
library(rmcqmcint)
unit.nsphere <- function(point) {
    if (sqrt(sum(point^2)) <= 1.0) {
        r <- 1.0
    } else {
        r <- 0.0
    }
    return(r)
}

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=100, s=4)
rs$mean
rs$absError
1 / 2 * pi^2 / 16 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=100, s=5, digitalNetID = 2)
rs$mean
rs$absError
8 / 15 * pi^2 / 32 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=100, s=4)
rs$mean
rs$absError
1 / 2 * pi^2 / 16 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=100, s=5)
rs$mean
rs$absError
8 / 15 * pi^2 / 32 # to compare true value.

## ------------------------------------------------------------------------
make.oscillatory <- function(a) {
	oscillatory <- function(point) {
		return(cos(sum(a * point)))
	}
	return(oscillatory)
}

## ------------------------------------------------------------------------
n <- 100
id <- 3
s <- 1000
m <- 10
p <- 0.99
a <- rep(pi / s, length=s)
osc <- make.oscillatory(a)
rs <- qmcint(osc, N=n, s=s, digitalNetID=id, m=m, probability=p)
rs$mean
rs$absError

