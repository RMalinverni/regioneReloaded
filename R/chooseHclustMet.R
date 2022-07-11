#' Choose HClust Method
#'
#' evaluate and choose the better method for clusterize a matrix using hclust function
#'
#' @usage chooseHclustMet(GM, scale = FALSE, vecMet = NA,distHC = "euclidean")
#'
#' @param GM matrix,  numerical matrix.
#' @param scale logic, if TRUE, the clusterization will be perform using the scale version of the matrix. (default = FALSE)
#' @param vecMet vector, vector of method that will be tested from the function
#' if NULL the follow methods will be tested "complete","average","single","ward.D2","median","centroid" and "mcquitty. (default = NULL)
#' @param distHC character, algorithm use for calculate distance. (default = "euclidean")
#'
#' @export
#' @keywords internal function


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
          "mcquitty")
    }

    mat_dist <- dist(x = GM, method = distHC)

    resMetList <- list()
    resMetVec <- vector()
    for (i in 1:length(vecMet)) {
      resMetList[[i]] <- hclust(d = mat_dist, method = vecMet[[i]])
      resMetVec[i] <- cor(x = mat_dist, cophenetic(resMetList[[i]]))
    }

    names(resMetList) <- vecMet
    names(resMetVec) <- vecMet

    name_model <- vecMet[which(resMetVec == max(resMetVec))]
    if (length(name_model) > 1) {
      name_model <- name_model[1]
    }

    model <- resMetList[[name_model]]

    print(paste0("method selected for hclustering: ", name_model))
    print(resMetVec)

    return(model)
  }


