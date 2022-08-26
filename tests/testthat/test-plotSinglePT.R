test_that("Argument checks", {
  expect_error(plotSinglePT(), "missing")
  expect_error(plotSinglePT("a"), "genoMatriXeR")
  expect_error(plotSinglePT(cw_Alien_ReG), "RS1")
  expect_error(plotSinglePT(cw_Alien_ReG, RS1 = "a", RS2 = "regA"), "RS1")
})

test_that("Returns the correct class without errors", {
  expect_s3_class(plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regA_05"), "ggplot")
  expect_s3_class(plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regC"), "ggplot")
})
