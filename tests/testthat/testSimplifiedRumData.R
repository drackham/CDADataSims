context("Check the Simplified RUM data simulation for correctness")

data <- simplifiedRUMData()
q <- hartzRoussosQLow()
J <- 5000
I <- 40
K <- 7

test_that("alphaJK has the proper dimensions", {
  expect_that(nrow(data$alphaJK), equals(J))
  expect_that(ncol(data$alphaJK), equals(K))
})

test_that("alphaJK has correct proportions of mastery", {
  expect_that(mean(data$alphaJK[,1]), equals(.3, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,2]), equals(.4, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,3]), equals(.45, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,4]), equals(.5, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,5]), equals(.55, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,6]), equals(.6, tolerance = 0.1, scale=1))
  expect_that(mean(data$alphaJK[,7]), equals(.65, tolerance = 0.1, scale=1))
})

test_that("iParamsLow has proper dimensions", {
  expect_that(nrow(data$iParamsLow), equals(I))
  expect_that(ncol(data$iParamsLow), equals(K)) # 7 r*
})


test_that("xMat is correctly formatted", {
  expect_that(nrow(data$xMat), equals(I))
  expect_that(ncol(data$xMat), equals(J))
})

test_that("N is the correct length", {
  expect_that(data$N, equals(J*I))
})

test_that("jj is correctly formatted", {
  expect_that(length(data$jj), equals(J*I))
})

test_that("ii is correctly formatted", {
  expect_that(length(data$ii), equals(J*I))
})
