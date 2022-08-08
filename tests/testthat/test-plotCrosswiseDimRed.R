test_that("Argument checks", {
  expect_error(plotCrosswiseDimRed(), "missing")
  expect_error(plotCrosswiseDimRed(cw_Alien_ReG), "empty")
  expect_error(plotCrosswiseDimRed("a"), "genoMatriXeR")
})

test_that("Returns the correct class", {
  cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReG), "ggplot")
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReG, return_table = TRUE), "data.frame")
})
