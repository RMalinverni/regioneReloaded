test_that("Argument checks", {
  expect_error(plotLocalZScoreMatrix(), "missing")
  expect_error(plotLocalZScoreMatrix("a"), "multiLocalZScore")
  expect_error(plotLocalZScoreMatrix(mLZ_regA_ReG), "empty")
})

test_that("Returns the correct class without errors", {
  mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)
  # Defaults
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG), "ggplot")
  # Matrix type
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG, matrix_type = "correlation"), "ggplot")
  # Maxval
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG, maxVal = 3), "ggplot")
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG, maxVal = -3), "ggplot")
  # Highlight
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG, highlight = c("regA_02", "regD")), "ggplot")
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG, highlight = c("regA_02", "regD"), highlight_max = TRUE), "ggplot")
})
