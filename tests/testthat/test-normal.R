context("Monte-Carlo and Quasi Monte-Carlo Integration: qmcint test normal")
library(rmcqmcint)
library(RSQLite)

unit.nsphere <- function(point) {
	if (sqrt(sum(point^2)) <= 1.0) {
		r <- 1.0
	} else {
		r <- 0.0
	}
	return(r)
}

# 1/16 Volume of S^4
v416 <- pi^2 / 2.0 / 16.0
# 1/32 Volume of S^5
v532 <- 8.0 * pi^2 / 15.0 / 32.0

test_that("qmcint normal case 1", {
        n <- 100
        s <- 4
	rs <- qmcint(integrand=unit.nsphere, N=n, s=s)
	expect_equal(rs$mean, expected = v416, tolerance = 2*rs$absError)
})

test_that("qmcint normal case 2", {
        n <- 100
        id <- 2
        s <- 5
        m <- 10
        p <- 0.99
	rs <- qmcint(unit.nsphere, n, s, id, m, p)
	expect_equal(rs$mean, expected = v532, tolerance = 2*rs$absError)
})


test_that("mcint normal case 1", {
        n <- 100
        s <- 4
	rs <- mcint(unit.nsphere, n, s)
	expect_equal(rs$mean, expected = v416, tolerance = rs$absError)
})

#test_that("mcint normal case 2", {
#        n <- 1000
#        s <- 5
#        m <- 10
#        p <- 0.99
#	rs <- mcint(unit.nsphere, n, s, m, p)
#	expect_equal(rs$mean, expected = v532, tolerance = rs$absError)
#})
