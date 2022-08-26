test_that("Argument checks", {
  expect_error(plotCrosswiseDimRed(), "missing")
  expect_error(plotCrosswiseDimRed("a"))
  expect_error(plotCrosswiseDimRed(cw_Alien_ReR), "empty")
})

test_that("Returns the correct class without errors", {
  cw_Alien_ReR <- makeCrosswiseMatrix(cw_Alien_ReR)
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR), "ggplot")
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, return_table = TRUE, return_plot = FALSE), "data.frame")
  # Different clustering methods
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, clust_met = "pam"), "ggplot")
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, clust_met = "kmeans"), "ggplot")
  # Different plot types
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, type = "tSNE"), "ggplot")
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, type = "UMAP"), "ggplot")
  # Graphical parameters
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, ellipse = TRUE), "ggplot")
  expect_warning(plotCrosswiseDimRed(cw_Alien_ReR, ellipse = TRUE, emphasize = TRUE))
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, ellipse = TRUE,
                                      emphasize = TRUE,
                                      listRS = list(regA = "regA", regAB = "regAB")), "ggplot")
  expect_s3_class(plotCrosswiseDimRed(cw_Alien_ReR, ellipse = TRUE,
                                      emphasize = TRUE,
                                      listRS = list(regA = "regA", regAB = "regAB"),
                                      labAll = TRUE), "ggplot")
})
