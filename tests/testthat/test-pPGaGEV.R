test_that("test of pPGaGEV", {
  x <- c(1:5)
  y <- x
  expect_equal(pPGaGEV(x,2,1,0.5,0.5,0.5,.5), pPGaGEV(y,2,1,0.5,0.5,0.5,.5))
})
