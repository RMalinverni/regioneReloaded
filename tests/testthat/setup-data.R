# Load sample data included in the package

data("cw_Alien")

# Define small subsets of the region set lists to test crosswisePermTest() with
# a small computation time

sel <- c("regA", "regB", "regAB")
sel_br <- c("regA_br", "regB_br", "regAB_br")

AlienRSList_narrow_small <- AlienRSList_narrow[sel]
AlienRSList_broad_small <- AlienRSList_broad[sel]
