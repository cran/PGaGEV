test_that("test of dPGaGEV", {
  x <- c(1:5)
  y <- x
  expect_equal(dPGaGEV(x,2,1,0.5,0.5,0.5,0.5), dPGaGEV(y,2,1,0.5,0.5,0.5,0.5))
})
