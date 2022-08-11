test_that("getMultiEvaluation retrieves the correct data", {
  expect_snapshot(getMultiEvaluation(cw_Alien_ReG))
  expect_snapshot(getMultiEvaluation(cw_Alien_ReR))
  expect_snapshot(getMultiEvaluation(mLZ_regA_ReG))

  # With specific region set names
  expect_snapshot(getMultiEvaluation(cw_Alien_ReG, namesRS = c("regA", "regB")))
  expect_snapshot(getMultiEvaluation(mLZ_regA_ReG, namesRS = c("regAB", "regB")))
})
