context("Check the RUM Hartz Roussos data simulation for correctness")

data <- RUMhartzRoussosData()
q <- hartzRoussosQLow()
JJ <- 1500
II <- 40
KK <- 7

test_that("alphaJK was simulated", {
  expect_that(data$alphaJK, is_a("matrix"))
})

test_that("alphaJK has the proper dimensions", {
  expect_that(nrow(data$alphaJK), equals(JJ))
  expect_that(ncol(data$alphaJK), equals(KK))
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
  expect_that(nrow(data$iParamsLow), equals(II))
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

  expect_that(nrow(q*data$iParamsLow[,c(2:8)]),equals(II))
  expect_that(ncol(q*data$iParamsLow[,c(2:8)]),equals(KK))

  expect_that(sum(q[,1]*data$iParamsLow[,2], na.rm=TRUE), equals(3.643))
  expect_that(sum(q[,2]*data$iParamsLow[,3], na.rm=TRUE), equals(2.933))
  expect_that(sum(q[,3]*data$iParamsLow[,4], na.rm=TRUE), equals(4.104))
  expect_that(sum(q[,4]*data$iParamsLow[,5], na.rm=TRUE), equals(2.447)) # modified
  expect_that(sum(q[,5]*data$iParamsLow[,6], na.rm=TRUE), equals(2.48)) # modified
  expect_that(sum(q[,6]*data$iParamsLow[,7], na.rm=TRUE), equals(2.229)) # modified
  expect_that(sum(q[,7]*data$iParamsLow[,8], na.rm=TRUE), equals(2.023))
})

test_that("y is correctly formatted", {
  expect_that(nrow(data$y), equals(JJ))
  expect_that(ncol(data$y), equals(II))
})

test_that("y has correct proportions of correct responses", {
  expect_equal(mean(data$y[,1]),  expected = .34, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,2]),  expected = .22, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,3]),  expected = .24, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,4]),  expected = .54, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,5]),  expected = .27, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,6]),  expected = .39, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,7]),  expected = .31, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,8]),  expected = .28, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,9]),  expected = .26, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,10]), expected = .39, tolerance = 0.2, scale=1)

  expect_equal(mean(data$y[,11]), expected = .38, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,12]), expected = .49, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,13]), expected = .30, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,14]), expected = .44, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,15]), expected = .22, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,16]), expected = .36, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,17]), expected = .39, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,18]), expected = .17, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,19]), expected = .29, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,20]), expected = .39, tolerance = 0.2, scale=1)

  expect_equal(mean(data$y[,21]), expected = .44, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,22]), expected = .37, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,23]), expected = .35, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,24]), expected = .39, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,25]), expected = .28, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,26]), expected = .54, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,27]), expected = .56, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,28]), expected = .29, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,29]), expected = .24, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,30]), expected = .43, tolerance = 0.2, scale=1)

  expect_equal(mean(data$y[,31]), expected = .21, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,32]), expected = .33, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,33]), expected = .39, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,34]), expected = .24, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,35]), expected = .21, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,36]), expected = .40, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,37]), expected = .32, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,38]), expected = .32, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,39]), expected = .20, tolerance = 0.2, scale=1)
  expect_equal(mean(data$y[,40]), expected = .45, tolerance = 0.2, scale=1)
})
