context("Check the Hartz Roussos data simulation for correctness")

data <- hartzRoussosData()
q <- hartzRoussosQLow()

test_that("alphaJK was simulated", {
  expect_that(data$alphaJK, is_a("matrix"))
})

test_that("alphaJK has the proper dimensions", {
  expect_that(nrow(data$alphaJK), equals(1500))
  expect_that(ncol(data$alphaJK), equals(7))
})

test_that("alphaJK has correct proportions of mastery", {
  expect_that(mean(data$alphaJK[,1]), equals(.3, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,2]), equals(.4, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,3]), equals(.45, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,4]), equals(.5, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,5]), equals(.55, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,6]), equals(.6, tolerance = 0.1))
  expect_that(mean(data$alphaJK[,7]), equals(.65, tolerance = 0.1))
})

test_that("iParamsLow has proper dimensions", {
  expect_that(nrow(data$iParamsLow), equals(40))
  expect_that(ncol(data$iParamsLow), equals(9)) # 1 pi 7 r and 1 c = 9
})

test_that("iParamsLow rows are correctly specified", {
  # Note 1/13/16: iParamsLow[4,9], [5,25], [6,13] and [6,20] are incorrectly specified in
  # The Fusion Model for Skills Diagnosis: Blending Theory with Practicality" (2008).

  expect_that(sum(data$iParamsLow[1,], na.rm=TRUE), equals(2.641))
  expect_that(sum(data$iParamsLow[2,], na.rm=TRUE), equals(1.136))
  expect_that(sum(data$iParamsLow[3,], na.rm=TRUE), equals(3.012))
  expect_that(sum(data$iParamsLow[4,], na.rm=TRUE), equals(2.321))
  expect_that(sum(data$iParamsLow[5,], na.rm=TRUE), equals(3.351))
  expect_that(sum(data$iParamsLow[6,], na.rm=TRUE), equals(4.485))
  expect_that(sum(data$iParamsLow[7,], na.rm=TRUE), equals(3.697))
  expect_that(sum(data$iParamsLow[8,], na.rm=TRUE), equals(2.048))
  expect_that(sum(data$iParamsLow[9,], na.rm=TRUE), equals(2.008)) # modified
  expect_that(sum(data$iParamsLow[10,], na.rm=TRUE), equals(1.708))

  expect_that(sum(data$iParamsLow[11,], na.rm=TRUE), equals(1.26))
  expect_that(sum(data$iParamsLow[12,], na.rm=TRUE), equals(1.653))
  expect_that(sum(data$iParamsLow[13,], na.rm=TRUE), equals(3.707)) # modified
  expect_that(sum(data$iParamsLow[14,], na.rm=TRUE), equals(3.107))
  expect_that(sum(data$iParamsLow[15,], na.rm=TRUE), equals(1.369))
  expect_that(sum(data$iParamsLow[16,], na.rm=TRUE), equals(1.872))
  expect_that(sum(data$iParamsLow[17,], na.rm=TRUE), equals(2.094))
  expect_that(sum(data$iParamsLow[18,], na.rm=TRUE), equals(4.195))
  expect_that(sum(data$iParamsLow[19,], na.rm=TRUE), equals(1.794))
  expect_that(sum(data$iParamsLow[20,], na.rm=TRUE), equals(2.842)) # modified

  expect_that(sum(data$iParamsLow[21,], na.rm=TRUE), equals(2.772))
  expect_that(sum(data$iParamsLow[22,], na.rm=TRUE), equals(1.159))
  expect_that(sum(data$iParamsLow[23,], na.rm=TRUE), equals(1.315))
  expect_that(sum(data$iParamsLow[24,], na.rm=TRUE), equals(3.14))
  expect_that(sum(data$iParamsLow[25,], na.rm=TRUE), equals(2.805)) # modified
  expect_that(sum(data$iParamsLow[26,], na.rm=TRUE), equals(2.837))
  expect_that(sum(data$iParamsLow[27,], na.rm=TRUE), equals(3.95))
  expect_that(sum(data$iParamsLow[28,], na.rm=TRUE), equals(2.764))
  expect_that(sum(data$iParamsLow[29,], na.rm=TRUE), equals(3.092))
  expect_that(sum(data$iParamsLow[30,], na.rm=TRUE), equals(2.965))

  expect_that(sum(data$iParamsLow[31,], na.rm=TRUE), equals(3.246))
  expect_that(sum(data$iParamsLow[32,], na.rm=TRUE), equals(3.261))
  expect_that(sum(data$iParamsLow[33,], na.rm=TRUE), equals(1.657))
  expect_that(sum(data$iParamsLow[34,], na.rm=TRUE), equals(2.661))
  expect_that(sum(data$iParamsLow[35,], na.rm=TRUE), equals(4.109))
  expect_that(sum(data$iParamsLow[36,], na.rm=TRUE), equals(3.803))
  expect_that(sum(data$iParamsLow[37,], na.rm=TRUE), equals(3.915))
  expect_that(sum(data$iParamsLow[38,], na.rm=TRUE), equals(1.639))
  expect_that(sum(data$iParamsLow[39,], na.rm=TRUE), equals(3.52))
  expect_that(sum(data$iParamsLow[40,], na.rm=TRUE), equals(2.60))

})

test_that("iParamsLow columns are correctly specified", {
  expect_that(sum(data$iParamsLow[,1], na.rm=TRUE), equals(35.549))
  expect_that(sum(data$iParamsLow[,2], na.rm=TRUE), equals(3.643))
  expect_that(sum(data$iParamsLow[,3], na.rm=TRUE), equals(2.933))
  expect_that(sum(data$iParamsLow[,4], na.rm=TRUE), equals(4.104))
  expect_that(sum(data$iParamsLow[,5], na.rm=TRUE), equals(2.447)) # modified
  expect_that(sum(data$iParamsLow[,6], na.rm=TRUE), equals(2.48)) # modified
  expect_that(sum(data$iParamsLow[,7], na.rm=TRUE), equals(2.229)) # modified
  expect_that(sum(data$iParamsLow[,8], na.rm=TRUE), equals(2.023))
  expect_that(sum(data$iParamsLow[,9], na.rm=TRUE), equals(52.102))
})

test_that("QLow and iParamsLow are congruent", {

  # If iParamsLow is correctly specified then multiplying the component vectors of
  # iParamsLow[,2:8] * Q should give the same marginal column sum

  expect_that(nrow(q*data$iParamsLow[,c(2:8)]),equals(40))
  expect_that(ncol(q*data$iParamsLow[,c(2:8)]),equals(7))

  expect_that(sum(q[,1]*data$iParamsLow[,2], na.rm=TRUE), equals(3.643))
  expect_that(sum(q[,2]*data$iParamsLow[,3], na.rm=TRUE), equals(2.933))
  expect_that(sum(q[,3]*data$iParamsLow[,4], na.rm=TRUE), equals(4.104))
  expect_that(sum(q[,4]*data$iParamsLow[,5], na.rm=TRUE), equals(2.447)) # modified
  expect_that(sum(q[,5]*data$iParamsLow[,6], na.rm=TRUE), equals(2.48)) # modified
  expect_that(sum(q[,6]*data$iParamsLow[,7], na.rm=TRUE), equals(2.229)) # modified
  expect_that(sum(q[,7]*data$iParamsLow[,8], na.rm=TRUE), equals(2.023))
})
