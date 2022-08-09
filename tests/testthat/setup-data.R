# Load sample data included in the package

data("cw_Alien")

# Define small subsets of the region set lists to test crosswisePermTest() with
# a small computation time

sel <- c("regA", "regA_08", "regB", "regB_08", "regAB")
sel_br <- c("regA_nbr", "regA_br_08", "regB_br", "regB_br_08", "regAB_br_08")

AlienRSList_narrow_small <- AlienRSList_narrow[sel]
AlienRSList_broad_small <- AlienRSList_broad[sel]
