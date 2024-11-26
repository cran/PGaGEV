test_that("test of rPGaGEV", {
 x <- rPGaGEV(30,2,1,0.5,0.5,0.5,0.5)
 y <- x
  expect_equal(x, y)
})
