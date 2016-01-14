context("Check the Hartz Roussos data simulation for correctness")

data <- hartzRoussosData()

test_that("alphaJK was simulated", {
  expect_that(data$alphaJK, is_a("matrix"))
})

test_that("alphaJK has the proper dimensions", {
  expect_that(nrow(data$alphaJK), equals(1500))
  expect_that(ncol(data$alphaJK), equals(7))
})

test_that("alphaJK hascorrect proportions of mastery", {
  expect_that(mean(data$alphaJK[,1]), equals(.3, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,2]), equals(.4, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,3]), equals(.45, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,4]), equals(.5, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,5]), equals(.55, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,6]), equals(.6, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,7]), equals(.65, tolerance = 0.1))
})
