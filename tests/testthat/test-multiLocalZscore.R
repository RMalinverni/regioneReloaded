test_that("multiLocalZscore: argument checks", {
  expect_error(multiLocalZscore(), "missing")
})

test_that("multiLocalZscore: different randomization strategies, returns the correct class without errors", {

  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "resampleRegions",
                                   evFUN = "numOverlaps",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "resampleGenome",
                                   evFUN = "numOverlaps",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "randomizeRegions",
                                   evFUN = "numOverlaps",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "circularRandomizeRegions",
                                   evFUN = "numOverlaps",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
})

test_that("multiLocalZscore: different evaluation functions, returns the correct class without errors", {

  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "resampleRegions",
                                   evFUN = "numOverlaps",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
  set.seed(42)

  expect_s4_class(multiLocalZscore(A = AlienRSList_narrow_small$regA,
                                   Blist = AlienRSList_narrow_small,
                                   ranFUN = "resampleRegions",
                                   evFUN = "meanDistance",
                                   window = 100,
                                   step = 20,
                                   max_pv = 1,
                                   genome = AlienGenome), "multiLocalZScore")
})
