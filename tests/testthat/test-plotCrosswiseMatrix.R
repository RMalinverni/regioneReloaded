test_that("Argument checks", {
  expect_error(plotCrosswiseMatrix(), "missing")
  expect_error(plotCrosswiseMatrix(cw_Alien_ReG), "empty")
  expect_error(plotCrosswiseMatrix("a"), "genoMatriXeR")
})

test_that("Returns the correct class without errors", {
  cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
  # Defaults
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG), "ggplot")
  # Matrix type and cor
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation"), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation", cor = "col"), "ggplot")
  # Maxval
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, maxVal = "max"), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, maxVal = 3), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, maxVal = -3), "ggplot")
  # Colors and interpolate
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, lineColor = "white", colMatrix = c("darkorange", "darkred", "black")), "ggplot")
  expect_s3_class(plotCrosswiseMatrix(cw_Alien_ReG, interpolate = TRUE), "ggplot")
})
