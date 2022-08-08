test_that("Argument checks", {
  expect_error(plotSingleLZ(), "missing")
  expect_error(plotSingleLZ("a"), "multiLocalZScore")
  expect_error(plotSingleLZ(mLZ_regA_ReG), "empty")
  mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)
  expect_error(plotSingleLZ(mLZ_regA_ReG), "RS")
})

test_that("Returns the correct class", {
  mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)
  expect_s3_class(plotSingleLZ(mLZ_regA_ReG, RS = c("regD", "regD_02", "regA", "regAB_04")), "ggplot")
})
