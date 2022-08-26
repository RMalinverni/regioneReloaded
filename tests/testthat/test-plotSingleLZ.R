test_that("Argument checks", {
  expect_error(plotSingleLZ(), "missing")
  expect_error(plotSingleLZ("a"), "multiLocalZScore")
  expect_error(plotSingleLZ(mLZ_regA_ReG), "missing")
  expect_error(plotSingleLZ(mLZ_regA_ReG, RS = "a"), "names")
})

test_that("Returns the correct class without errors", {
  expect_s3_class(plotSingleLZ(mLZ = mLZ_regA_ReG,RS =c("regA","regA_02","regA_06","regA_08","regD")), "ggplot")
  expect_s3_class(plotSingleLZ(mLZ = mLZ_regA_ReG,RS =c("regA","regA_02","regA_06","regA_08","regD"), smoothing = TRUE), "ggplot")
  expect_s3_class(plotSingleLZ(mLZ = mLZ_regA_ReG,RS =c("regA","regA_02","regA_06","regA_08","regD"), colPal = c("black", "red")), "ggplot")
  expect_s3_class(plotSingleLZ(mLZ = mLZ_regA_ReG,RS =c("regA","regA_02","regA_06","regA_08","regD"), labValues = FALSE), "ggplot")
  expect_s3_class(plotSingleLZ(mLZ = mLZ_regA_ReG,RS =c("regA","regA_02","regA_06","regA_08","regD"), labMax = FALSE), "ggplot")
})
