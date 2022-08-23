test_that("Argument checks", {
  expect_error(plotCrosswiseMatrix(), "missing")
  expect_error(plotCrosswiseMatrix(cw_Alien_ReG), "empty")
  expect_error(plotCrosswiseMatrix("a"), "genoMatriXeR")
})

test_that("Returns the correct class", {
  cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation"), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG), "ggplot")
})
