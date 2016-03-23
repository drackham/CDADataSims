context("Check the simple Q matrix for correctness")

q <- simpleQ()

test_that ("Simple Q is a matrix", {
  expect_that(q, is_a("matrix"))
})

test_that ("Simple Q has correct dimensions", {
  expect_that(nrow(q), equals(30))
  expect_that(ncol(q), equals(2))
})
