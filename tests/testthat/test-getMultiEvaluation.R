test_that("getMultiEvaluation retrieves the correct data", {
  expect_snapshot(getMultiEvaluation(cw_Alien_ReG))
  expect_snapshot(getMultiEvaluation(mLZ_regA_ReG))
})
