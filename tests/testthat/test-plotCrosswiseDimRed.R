test_that("Argument checks", {
  expect_error(plotCrosswiseDimRed(), "missing")
  expect_error(plotCrosswiseDimRed("a"))
})
