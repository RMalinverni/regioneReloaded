#' Plot Crosswise Dimensionality Reduction
#'
#' Plot a Dimensionality Reduction visualization of gMXR object (or matrix) selecting between different algorithms (PCA, tSNE and UMAP)
#' set the result will be store in a gMXR S4 class
#'
#' @usage plotCrosswiseDimRed(mPt, type = "PCA", GM_clust = NA, nc = 5, listGene = NULL, main = "", size_labels = 2, underline = FALSE, perplexity = 10, theta = 0.1, ellipse=TRUE)
#'
#'
#' @param mpt object of class genoMatriXeR or numeric matrix
#' @param type character, Dimensionality Reduction algorithm selected between ("PCA", "tSNE", "UMAP"). (default  = "PCA")
#' @param GM_clust vector, vector of cluster using to clusterize the matrix, if NA will the matrix will be clusterize using \code{\link{kmeans}} function. (default = NA)
#' @param nc numeric, nuber of cluster. (default = 5)
#' @param listGene list of vector, if is not NULL this will be a list of element highlighted in the graph. (default = NULL)
#' @param main character, name of the graph. (defaut = "")
#' @param size_labels numeric, size for element names in the graph. (default = 2)
#' @param underline logic, if present listGene and underline is TRUE only the cluster in which the element of listGene are present will be highlighted. (default = FALSE)
#' @param perplexity numeric, if type = "tSNE" value of perplexity for the function \code{\link{Rtsne}}. (default = 10)
#' @param theta numeric, if type = "tSNE" value of theta for the function \code{\link{Rtsne}}. (default = 0.1)
#' @param ellipse logic, if TRUE ellipses will be drawn around the clusters. (default = FALSE)
#'
#' @export plotCrosswiseDimRed
#' @import ggplot2
#' @import Rtsne
#' @import umap
#' @import ggrepel
#'
#'
#'
plotCrosswiseDimRed <-
  function(mPt,
           type = "PCA",
           GM_clust = NA,
           nc = 5,
           listGene = NULL,
           main = "",
           size_labels = 2,
           underline = FALSE,
           perplexity = 10,
           theta = 0.1,
           ellipse=TRUE,
           labMaxOverlap=100,
           ...) {

    if (class(mPt)=="genoMatriXeR"){
      GM <- mPt@matrix$GMat
    }

    if (is.matrix(mPt)){
      GM <- mPt
    }

    if (is.na(GM_clust)) {
      GM_clust <- kmeans(GM, centers = nc)

    }

    if (type == "PCA") {

      pdr_out = princomp(GM, scores = TRUE)
      pdr_df = data.frame(pdr_out$scores)
      pdr_df <- pdr_df[, c("Comp.1", "Comp.2")]
      colnames(pdr_df) <- c("x", "y")
      pdr_df$Name <- rownames(GM)

    }

    if (type == "tSNE") {

      pdr_out <- Rtsne(GM, perplexity = perplexity, theta = theta,check_duplicates = FALSE)
      pdr_df <- data.frame(x = pdr_out$Y[, 1],
                   y = pdr_out$Y[, 2],
                   Name = rownames(GM))

    }

    if (type == "UMAP"){
      pdr_out <- umap(GM)
      pdr_df <-
        data.frame(x = pdr_out$layout[,1],
                   y = pdr_out$layout[,2],
                   Name = rownames(GM))
    }


    pdr_df$clust <-
      paste0("clust_", as.factor(GM_clust$cluster))

    pdr_df$clust1 <- rep("none", nrow(pdr_df))

    for (i in 1:length(listGene)) {
      for (x in 1:length(listGene[[i]])){
        pdr_df$clust1[pdr_df$Name==listGene[[i]][x]]<-names(listGene)[i]
      }
    }

    pdr_df$clust2 <- rep("none", nrow(pdr_df))
    sel_clust<-pdr_df$clust[pdr_df$clust1!="none"]

    for (i in 1:length(sel_clust)){
      pdr_df$clust2[pdr_df$clust == sel_clust[i]]<- sel_clust[i]
    }

    if (!is.null(listGene)){
      pdr_df$clust<-pdr_df$clust1
      if (underline==TRUE){
        pdr_df$clust<-pdr_df$clust2
      }
    }

    p <-
      ggplot(pdr_df, aes(
        x = x,
        y = y,
        label = Name,
        color = clust
      )) +
      geom_point() +

      geom_text_repel(
        size  = size_labels,
        aes(label = Name),
        max.overlaps=labMaxOverlap,
        point.padding = 0.5,
        segment.color = "grey"
      )


    if(ellipse==TRUE){
      p <- p + stat_ellipse(type = "t",
                     geom = "polygon",
                     alpha = 0.15)
    }

    if(type=="PCA"){
      p <- p + labs(title = "PCA plot" ,
                    subtitle = main
                   )
    }

    if(type=="tSNE"){
      p <- p + labs(title = "tSNE plot" ,
        subtitle = main,
        caption = paste0("perplexity: ", perplexity, " theta: ", theta))
    }

    if(type=="UMAP"){
      p <- p + labs(title = "UMAP plot" ,
                    subtitle = main)
    }

    return(p)

  }

