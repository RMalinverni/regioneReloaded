test_that("getMatrix retrieves the correct data", {
  expect_warning(getMatrix(cw_Alien_ReG))
  expect_warning(getMatrix(mLZ_regA_ReG))
  cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
  mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)
  expect_snapshot(getMatrix(cw_Alien_ReG))
  expect_snapshot(getMatrix(mLZ_regA_ReG))
})
