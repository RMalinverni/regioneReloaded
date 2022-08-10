test_that("crosswisePermTest: argument checks", {
  expect_error(crosswisePermTest(), "missing")
})

test_that("crosswisePermTest: different randomization functions", {

  set.seed(42)

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = FALSE,
      genome = AlienGenome,
      per.chromosome = TRUE,
      ranFUN = "resampleGenome",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = FALSE,
      genome = AlienGenome,
      ranFUN = "resampleRegions",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = TRUE,
      genome = AlienGenome,
      ranFUN = "randomizeRegions",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = TRUE,
      genome = AlienGenome,
      ranFUN = "circularRandomizeRegions",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )
})

test_that("crosswisePermTest: different evaluation functions", {

  set.seed(42)

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = FALSE,
      genome = AlienGenome,
      per.chromosome = TRUE,
      ranFUN = "circularRandomizeRegions",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = FALSE,
      genome = AlienGenome,
      ranFUN = "circularRandomizeRegions",
      evFUN = "meanDistance",
      ntimes = 5,
      mc.cores = 2
    )
  )
})

test_that("crosswisePermTest: resampling and fraction", {

  set.seed(42)

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = TRUE,
      fraction = 0.25,
      genome = AlienGenome,
      per.chromosome = TRUE,
      ranFUN = "resampleGenome",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )

  expect_snapshot(
    crosswisePermTest(
      Alist = AlienRSList_narrow_small,
      Blist = AlienRSList_narrow_small,
      sampling = TRUE,
      fraction = 0.5,
      genome = AlienGenome,
      per.chromosome = TRUE,
      ranFUN = "resampleGenome",
      evFUN = "numOverlaps",
      ntimes = 5,
      mc.cores = 2
    )
  )
})
