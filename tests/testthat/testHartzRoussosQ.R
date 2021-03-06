context("Check the Hartz Roussos Q matrix for correctness")

q <- hartzRoussosQLow()

test_that ("Hartz Roussos Q is a matrix", {
  expect_that(q, is_a("matrix"))
})

test_that ("Hartz Roussos Q has correct dimensions", {
  expect_that(nrow(q), equals(40))
  expect_that(ncol(q), equals(7))
})

test_that ("Hartz Roussos Q has correct marginal row sums", {
  expect_that (sum(q[1,]),  equals(2))
  expect_that (sum(q[2,]),  equals(1))
  expect_that (sum(q[3,]),  equals(2))
  expect_that (sum(q[4,]),  equals(1))
  expect_that (sum(q[5,]),  equals(2))
  expect_that (sum(q[6,]),  equals(3))
  expect_that (sum(q[7,]),  equals(3))
  expect_that (sum(q[8,]),  equals(2))
  expect_that (sum(q[9,]),  equals(2))
  expect_that (sum(q[10,]), equals(1))

  expect_that (sum(q[11,]), equals(1))
  expect_that (sum(q[12,]), equals(1))
  expect_that (sum(q[13,]), equals(3))
  expect_that (sum(q[14,]), equals(1))
  expect_that (sum(q[15,]), equals(2))
  expect_that (sum(q[16,]), equals(2))
  expect_that (sum(q[17,]), equals(2))
  expect_that (sum(q[18,]), equals(4))
  expect_that (sum(q[19,]), equals(2))
  expect_that (sum(q[20,]), equals(1))

  expect_that (sum(q[21,]), equals(1))
  expect_that (sum(q[22,]), equals(1))
  expect_that (sum(q[23,]), equals(1))
  expect_that (sum(q[24,]), equals(2))
  expect_that (sum(q[25,]), equals(2))
  expect_that (sum(q[26,]), equals(1))
  expect_that (sum(q[27,]), equals(2))
  expect_that (sum(q[28,]), equals(3))
  expect_that (sum(q[29,]), equals(4))
  expect_that (sum(q[30,]), equals(2))

  expect_that (sum(q[31,]), equals(3))
  expect_that (sum(q[32,]), equals(2))
  expect_that (sum(q[33,]), equals(1))
  expect_that (sum(q[34,]), equals(3))
  expect_that (sum(q[35,]), equals(3))
  expect_that (sum(q[36,]), equals(2))
  expect_that (sum(q[37,]), equals(2))
  expect_that (sum(q[38,]), equals(2))
  expect_that (sum(q[39,]), equals(3))
  expect_that (sum(q[40,]), equals(2))
})

test_that ("Hartz Roussos Q has correct marginal column sums", {
  expect_that (sum(q[,1]), equals(11))
  expect_that (sum(q[,2]), equals(10))
  expect_that (sum(q[,3]), equals(11))
  expect_that (sum(q[,4]), equals(12))
  expect_that (sum(q[,5]), equals(11))
  expect_that (sum(q[,6]), equals(12))
  expect_that (sum(q[,7]), equals(13))
})
