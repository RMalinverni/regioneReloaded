test_that("getParameters argument checks", {
  expect_error(getParameters(), "missing")
  expect_error(getParameters("a"), "genoMatriXeR")
})

test_that("getParameters retrieves the correct data", {
  expect_snapshot(getParameters(cw_Alien_ReG))
  expect_snapshot(getParameters(mLZ_regA_ReG))
})
