test_that("Argument checks", {
  expect_error(plotSinglePT(), "missing")
  expect_error(plotSinglePT("a"), "genoMatriXeR")
  expect_error(plotSinglePT(cw_Alien_ReG), "empty")
})

test_that("Returns the correct class", {
  cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
  expect_s3_class(plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regA_05"), "ggplot")
})
