#' chooseHclustMet
#'
#' @description
#'
#' Evaluate and choose the best method for clusterizing a matrix using the [hclust()] function
#'
#' @usage chooseHclustMet(GM, scale = FALSE, vecMet = NULL ,distHC = "euclidean")
#'
#'
#'
#' @param GM matrix,  numerical matrix.
#' @param scale logical, if TRUE, the clusterization will be performed using the scaled matrix. (default = FALSE)
#' @param vecMet vector, vector of methods that will be tested in the function.
#' If NULL, the following methods will be tested: "complete","average","single","ward.D2","median","centroid" and "mcquitty. (default = NULL)
#' @param distHC character, the distance measure to be used from those available in [dist()] . (default = "euclidean")
#'
#' @return
#'
#' an object of class "hclust" \code{\link[stats]{"hclust"}}
#'
#' @seealso
#'
#' \code{\link[stats]{"hclust"}}
#'
#' @examples
#'
#' M1 <- matrix(1:18, nrow = 6, ncol = 3)
#' set.seed(42)
#' M2 <- matrix(sample(100, 18), nrow = 6, ncol = 3)
#' GM <- cbind(M1, M2)
#'
#' chooseHclustMet(GM)
#'
#' @export chooseHclustMet
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats cor
#' @importFrom stats cophenetic
#' @importFrom methods show
#'



chooseHclustMet <-
    function(GM,
        scale = FALSE,
        vecMet = NULL,
        distHC = "euclidean") {
        if (scale == TRUE) {
            GM <- scale(GM)
        }

        if (is.null(vecMet)) {
            vecMet <-
            c("complete",
            "average",
            "single",
            "ward.D2",
            "median",
            "centroid",
            "mcquitty"
            )
       }

    mat_dist <- stats::dist(x = GM, method = distHC)

    resMetList <- list()
    resMetVec <- vector()

    for (i in seq_along(vecMet)) {
      resMetList[[i]] <- stats::hclust(d = mat_dist, method = vecMet[[i]])
      resMetVec[i] <-
        stats::cor(x = mat_dist, stats::cophenetic(resMetList[[i]]))
    }

    names(resMetList) <- vecMet
    names(resMetVec) <- vecMet

    name_model <- vecMet[which(resMetVec == max(resMetVec))]

    if (length(name_model) > 1) {
      name_model <- name_model[1]
    }

    model <- resMetList[[name_model]]

    methods::show(paste0("method selected for hclustering: ", name_model))
    methods::show(resMetVec)

    return(model)
  }
