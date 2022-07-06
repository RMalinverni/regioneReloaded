#' Plot Crosswise Dimensionality Reduction
#'
#' Plot a Dimensionality Reduction visualization of gMXR object (or matrix) selecting between different algorithms (PCA, tSNE and UMAP)
#' set the result will be store in a gMXR S4 class
#'
#' @usage plotCrosswiseDimRed(mPt, type = "PCA", GM_clust = NA, nc = 5, listRS = NULL, main = "", size_labels = 2, emphasize = FALSE, perplexity = 10, theta = 0.1, ellipse=TRUE)
#'
#'
#' @param mpt object of class genoMatriXeR or numeric matrix
#' @param type character, Dimensionality Reduction algorithm selected between ("PCA", "tSNE", "UMAP"). (default  = "PCA")
#' @param GM_clust vector, vector of cluster using to clusterize the matrix, if NA will the matrix will be clusterize using \code{\link{kmeans}} function. (default = NA)
#' @param nc numeric, nuber of cluster. (default = 5)
#' @param listRS list of vector, if is not NULL this will be a list of element highlighted in the graph. (default = NULL)
#' @param main character, name of the graph. (defaut = "")
#' @param size_labels numeric, size for element names in the graph. (default = 2)
#' @param emphasize logic, if present listRS and emphasize is TRUE only the cluster in which the element of listRS are present will be highlighted. (default = FALSE)
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
#'
plotCrosswiseDimRed <-
  function(mPt,
           type = "PCA",
           GM_clust = NA,
           nc = 5,
           listRS = NULL,
           main = "",
           size_labels = 2,
           emphasize = FALSE,
           label_none = FALSE,
           perplexity = 10,
           theta = 0.1,
           ellipse=FALSE,
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

      pdr_out <- Rtsne(GM, perplexity = perplexity, theta = theta, check_duplicates = FALSE)
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

    for (i in 1:length(listRS)) {
      for (x in 1:length(listRS[[i]])){
        pdr_df$clust1[pdr_df$Name==listRS[[i]][x]]<-names(listRS)[i]
      }
    }

    pdr_df$clust2 <- rep("none", nrow(pdr_df))
    sel_clust<-pdr_df$clust[pdr_df$clust1 != "none"]

    for (i in 1:length(sel_clust)){
      pdr_df$clust2[pdr_df$clust == sel_clust[i]]<- sel_clust[i]
    }

    if (!is.null(listRS)){
      pdr_df$clust<-pdr_df$clust1

      if (emphasize==TRUE){
        pdr_df$clust<-pdr_df$clust2
        pdr_df_emph <- pdr_df[pdr_df$clust2 != "none",]
      }
    }

    p <-
      ggplot(pdr_df, aes(
        x = x,
        y = y,
        label = Name,
        color = clust
      )) +
      geom_point()

    if(emphasize & ellipse){ # ellipse only for emphasized clusters
      p <- p + stat_ellipse(data = pdr_df_emph,
                            type = "t",
                            geom = "polygon",
                            alpha = 0.15)
    } else if (ellipse){ # ellipse for all clusters
      p <- p + stat_ellipse(type = "t",
                            geom = "polygon",
                            alpha = 0.15)
    }

    if (label_none) { # label all clusters
      p <- p + geom_text_repel(size  = size_labels,
                               aes(label = Name),
                               max.overlaps=labMaxOverlap,
                               point.padding = 0.5,
                               segment.color = "grey")
    } else {
      p <- p + geom_text_repel(data = pdr_df_emph,
                               size  = size_labels,
                               aes(label = Name),
                               max.overlaps=labMaxOverlap,
                               point.padding = 0.5,
                               segment.color = "grey")
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

