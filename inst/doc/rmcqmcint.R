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
rs <- qmcint(integrand=unit.nsphere, N=1000, s=4)
rs$mean
rs$absError
1 / 2 * pi^2 / 16 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=1000, s=5, digitalNetID = 2)
rs$mean
rs$absError
8 / 15 * pi^2 / 32 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=1000, s=4)
rs$mean
rs$absError
1 / 2 * pi^2 / 16 # to compare true value.

## ------------------------------------------------------------------------
rs <- qmcint(integrand=unit.nsphere, N=1000, s=5)
rs$mean
rs$absError
8 / 15 * pi^2 / 32 # to compare true value.

