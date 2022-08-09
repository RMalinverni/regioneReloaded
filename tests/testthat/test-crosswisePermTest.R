test_that("crosswisePermTest", {
  set.seed(42)
  expect_snapshot(crosswisePermTest(
                    Alist = AlienRSList_narrow_small,
                    Blist = AlienRSList_narrow_small,
                    sampling = FALSE,
                    genome = AlienGenome,
                    per.chromosome = TRUE,
                    ranFUN = "resampleGenome",
                    evFUN = "numOverlaps",
                    ntimes= 10,
                    mc.cores = 2
                  ))
})
