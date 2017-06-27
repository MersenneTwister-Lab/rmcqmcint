context("Monte-Carlo and Quasi Monte-Carlo Integration: qmcint test DigitalNet")
library(rmcqmcint)
library(RSQLite)

test_that("test digitalnet dimMinMax", {
  rs <- digitalnet.dimMinMax(1)
  expect_true(rs[1] <= 4)
  expect_true(rs[2] >= 10)
  rs <- digitalnet.dimMinMax(2)
  expect_true(rs[1] <= 4)
  expect_true(rs[2] >= 10)
  rs <- digitalnet.dimMinMax(3)
  expect_true(rs[1] <= 2)
  expect_true(rs[2] >= 20000)
})

test_that("test digitalnet dimF2MinMax", {
  rs <- digitalnet.dimF2MinMax(1, 10)
  expect_true(rs[1] <= 10)
  expect_true(rs[2] >= 18)
  rs <- digitalnet.dimF2MinMax(2, 10)
  expect_true(rs[1] <= 10)
  expect_true(rs[2] >= 18)
  rs <- digitalnet.dimF2MinMax(3, 10)
  expect_true(rs[1] <= 10)
  expect_true(rs[2] >= 20)
})

test_that("test digitalnet points", {
  s <- 4
  m <- 10
  n <- 2^m
  matrix <- digitalnet.points(1, s, m, n)
  expect_equal(nrow(matrix), n)
  expect_equal(ncol(matrix), s)
  expect_true(all(matrix < 1))
  expect_true(all(matrix > 0))
})

test_that("test digitalnet points", {
  s <- 4
  m <- 11
  n <- 2^m
  matrix <- digitalnet.points(1, s, m, n, digitalShift=TRUE)
  expect_equal(nrow(matrix), n)
  expect_equal(ncol(matrix), s)
  expect_true(all(matrix < 1))
  expect_true(all(matrix > 0))
})
