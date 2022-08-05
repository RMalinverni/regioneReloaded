library(regioneReload)
data("cw_Alien")
# data("cw_Real")
x <- 2
y <- "a"
cw_Alien_ReG<-makeCrosswiseMatrix(cw_Alien_ReG, pvcut = 1)

plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "crosswise")
plotCrosswiseMatrix(cw_Alien_ReG, matrix_type = "correlation")
plotCrosswiseMatrix(x, matrix_type = "crosswise")
plotCrosswiseMatrix(y, matrix_type = "crosswise")

# plotSinglePT
plotSinglePT(cw_Alien_ReG, RS1 = "regA")
plotSinglePT(cw_Alien_ReG, RS1 = "regA", RS2 = "regX09")

plotSinglePT(cw_Alien_ReG, RS1 = "regA","regC")

p<-plotSinglePT(cw_Alien_ReG, RS1 = "regA","reg_no_A")

cw_Alien_ReG_s <- makeCrosswiseMatrix(cw_Alien_ReG,scale = TRUE)
lsRegSet<-list(regA="regA",regB="regB",regC="regC")

# Local Zscores

mlz_alien<-makeLZMatrix(mLZ_regA_ReG,normalize = TRUE,centralize = 3)
plotSingleLZ(mlz_alien, RS = c("regD", "regD_02", "regA", "regAB_04"), labMax = TRUE, smoothing = TRUE)
plotSingleLZ(mlz_alien, RS = c("regD", "regD_02", "regA"), labMax = TRUE, smoothing = TRUE)
plotSingleLZ(mlz_alien, RS = c("regA"), labMax = TRUE, smoothing = TRUE)



plotLocalZScoreMatrix(cw_Alien_RaR, maxVal = "max", highlight = c("regD", "regD_02", "regA"),
                      highlight_max = TRUE)
plotLocalZScoreMatrix(mlz_alien, maxVal = "max", highlight = c("regD", "regD_02", "regA"),
                      highlight_max = TRUE)



set.seed(67)
plotCrosswiseDimRed(cw_Alien_ReG, nc = 6, type="PCA", listRS = lsRegSet, ellipse = TRUE, emphasize = FALSE, labAll = TRUE, labSize = 3,
                    colPal = c("darkorange", "black", "red", "darkblue"))
plotCrosswiseDimRed(cw_Alien_ReG, nc = 6, type="PCA", ellipse = TRUE, emphasize = FALSE, labAll = TRUE, labSize = 3)


CW <- makeCrosswiseMatrix(CW)
plotCrosswiseDimRed(CW, nc = 25, type="UMAP",listRS = list(RAD21=c("CTCF_ENCFF237OKO")), ellipse = TRUE, emphasize = TRUE,
                    label_all = FALSE, label_size = 2, return_table = FALSE, clust_met = "pam")

mlZ_CTCF<-makeLZMatrix(mlZ_CTCF,normalize = TRUE,centralize =3)
plotLocalZScoreMatrix(mlZ_CTCF,
                      maxVal = "max",
                      labSize = 5,
                      main="CTCF_ENCFF237OKO",
                      smoothing = FALSE,
                      highlight = c("RAD21_ENCFF072UEX", "HNF1A_ENCFF227PWE", "YBX1_ENCFF920LEU"),
                      highlight_max = FALSE)

set.seed(42)
plotSingleLZ(mlZ_CTCF, RS = c("RAD21_ENCFF072UEX"))
plotSingleLZ(mlZ_CTCF, RS = c("HNF1A_ENCFF227PWE"))
plotSingleLZ(mlZ_CTCF, RS = c("RAD21_ENCFF072UE", "HNF1A_ENCFF227PWE", "YBX1_ENCFF920LEU"), normZS = TRUE, ylim = c(-5,3))
plotSingleLZ(mlZ_CTCF, RS = sample(names(CW@multiOverlaps), 5), normZS = TRUE, colPal = c("black", "darkgreen", "orange"),
             labMax = FALSE, labValues = FALSE)

mlZ_CTCF@multiLocalZscores$resumeTab

# Testing functions ----

library(regioneReloaded)
data("cw_Alien")

RS_small <- sample(AlienRSList_narrow, 5)

set.seed(42)
cw_Alien_RaR <-  crosswisePermTest(
  Alist = RS_small,
  Blist = RS_small,
  sampling = FALSE,
  genome = AlienGenome,
  per.chromosome=TRUE,
  ranFUN = "randomizeRegions",
  evFUN = "numOverlaps",
  ntimes= 100,
  mc.cores = 4
)


set.seed(42)
cw_Alien_ReR <-  crosswisePermTest(
  Alist = AlienRSList_narrow,
  Blist = AlienRSList_narrow,
  sampling = FALSE,
  genome = AlienGenome,
  per.chromosome=TRUE,
  ranFUN = "resampleRegions",
  evFUN = "numOverlaps",
  ntimes= 100,
  mc.cores = 4
)

set.seed(42)
cw_Alien_ReG <-  crosswisePermTest(
  Alist = AlienRSList_narrow,
  Blist = AlienRSList_narrow,
  sampling = FALSE,
  genome = AlienGenome,
  per.chromosome=TRUE,
  ranFUN = "resampleGenome",
  evFUN = "numOverlaps",
  ntimes= 100,
  mc.cores = 4
)

cw_Alien_RaR <- makeCrosswiseMatrix(cw_Alien_RaR)
cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)

head(getMatrix(cw_Alien_RaR))
getParameters(cw_Alien_RaR)
getMultiEvaluation(cw_Alien_RaR, c("regB_07", "X", "Y"))
getMultiEvaluation(mLZ_regA_ReG, c("regB_07", "X"))

plotCrosswiseMatrix(cw_Alien_ReG, lineColor = "darkblue", interpolate = FALSE,
                    matrix_type = "association", ord_mat = ord)

plotCrosswiseDimRed(cw_Alien_RaR, nc = 6, listRS = list(regA = "regA", regB = "regB"), labSize = 3, emphasize = TRUE, labAll = FALSE)

mLZ_regA_ReG <- makeLZMatrix(mLZ_regA_ReG)

plotLocalZScoreMatrix(mLZ_regA_ReG)
