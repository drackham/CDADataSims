context("Check the RUM Simple Q matrix data simulation for correctness")

data <- RUMSimpleQData()
q <- simpleQ()
J <- 1000
I <- 30
K <- 2

test_that("alphaJK was simulated", {
  expect_that(data$alphaJK, is_a("matrix"))
})

test_that("alphaJK has the proper dimensions", {
  expect_that(nrow(data$alphaJK), equals(J))
  expect_that(ncol(data$alphaJK), equals(K))
})

test_that("alphaJK has correct proportions of mastery", {
  expect_that(mean(data$alphaJK[,1]), equals(.7, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,2]), equals(.5, tolerance = 0.1, scale=1))
})

test_that("iParamsLow has proper dimensions", {
  expect_that(nrow(data$iParamsLow), equals(I))
  expect_that(ncol(data$iParamsLow), equals(K)) # 1 pi 7 r and 1 c = 9
})
