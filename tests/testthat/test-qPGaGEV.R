test_that("test of qPGaGEV", {
  x=c(1.2,1.3,1.4)
  p <- pPGaGEV(x,2,1,0.5,0.5,0.5,0.5)
  q <- p
  expect_equal(qPGaGEV(p,2,1,0.5,0.5,0.5,0.5), qPGaGEV(q,2,1,0.5,0.5,0.5,0.5))
})
