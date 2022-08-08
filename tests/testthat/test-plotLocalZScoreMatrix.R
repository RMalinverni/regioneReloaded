test_that("Argument checks", {
  expect_error(plotLocalZScoreMatrix(), "missing")
  expect_error(plotLocalZScoreMatrix("a"), "multiLocalZScore")
  expect_error(plotLocalZScoreMatrix(cw_Alien_ReG), "multiLocalZScore")
  expect_error(plotLocalZScoreMatrix(mLZ_regA_ReG), "empty")
})

test_that("Returns the correct class", {
  mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)
  expect_s3_class(plotLocalZScoreMatrix(mLZ_regA_ReG), "ggplot")
})
