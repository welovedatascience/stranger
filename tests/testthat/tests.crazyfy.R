

test_that("NA column leads to error", {
  data(iris)
  iris$allmiss <- NA
  expect_error(crazyfy(iris))
})

test_that("Invalid provided ID leads to error",{
  data(iris)
  iris$invaliID <- c(1,1, 3:(nrow(iris)))
  expect_error(crazyfy(iris,id="invalidID"))
})

