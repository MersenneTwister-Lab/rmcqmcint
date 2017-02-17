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
        rs <- qmcint(integrand=unit.nsphere, N=1000, s=4)
        rs$mean
        rs$absError
        pi^2 / 2.0 / 16.0 # to compare true value.

