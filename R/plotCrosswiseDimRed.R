#' Plot Crosswise Dimensionality Reduction
#'
#' Plot a Dimensionality Reduction visualization of gMXR object (or matrix) selecting between different algorithms (PCA, tSNE and UMAP)
#'
#' @usage plotCrosswiseDimRed(mPt, type = "PCA", GM_clust = NA, nc = 5, listRS = NULL, main = "", label_size = 2, emphasize = FALSE,
#' label_all = FALSE, labMaxOverlap = 100, ellipse = TRUE, perplexity = 10, theta = 0.1, return_table = FALSE)
#'
#'
#' @param mPT an object of class genoMatriXeR or numeric matrix.
#' @param type character, Dimensionality Reduction algorithm to use ("PCA", "tSNE", "UMAP"). (default  = "PCA")
#' @param GM_clust numeric, vector of clusters used to clusterize the matrix, if NA will the matrix will be clusterized using the \code{\link{kmeans}} function. (default = NA)
#' @param nc numeric, number of cluster to define if using the default kmeans method. (default = 5)
#' @param listRS list of vector, a list of names of regionset of interest to be highlighted in the graph. (default = NULL)
#' @param main character, title for the plot. (default = "")
#' @param label_size numeric, size for point labels in the plot, if 0 no labels will be plotted (default = 2)
#' @param emphasize logical, if listRS is not NULL and emphasize is TRUE only the cluster in which the elements of listRS are present will be highlighted. (default = FALSE)
#' @param label_all logical, if TRUE data points which are not in listRS when emphasize = TRUE are labelled. (default = FALSE)
#' @param labMaxOverlap numeric, max.overlaps for \code{\link{geom_text_repel}}. (default = 100)
#' @param ellipse logical, if TRUE ellipses will be drawn around the clusters. (default = FALSE)
#' @param perplexity numeric, if type = "tSNE" value of perplexity for the function \code{\link{Rtsne}}. (default = 10)
#' @param theta numeric, if type = "tSNE" value of theta for the function \code{\link{Rtsne}}. (default = 0.1)
#' @param return_table logical, if TRUE a table with the cluster assigned to each region is returned instead of the plot. (default = FALSE)
#'
#' @return A plot is created on the current graphics device or a table with cluster assignments is returned.
#'
#' @seealso \code{\link{crosswisePermTest}}
#'
#' @examples
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
#'
#' plotCrosswiseDimRed(cw_Alien_ReG, type = "PCA")
#'
#' plotCrosswiseDimRed(cw_Alien_ReG, type = "UMAP")
#'

#' @export plotCrosswiseDimRed
#' @import ggplot2
#' @import Rtsne
#' @import umap
#' @import ggrepel
#'

plotCrosswiseDimRed <-
  function(mPT,
           type = "PCA",
           GM_clust = NA,
           nc = 5,
           listRS = NULL,
           main = "",
           label_size = 2,
           emphasize = FALSE,
           label_all = FALSE,
           labMaxOverlap = 100,
           ellipse = FALSE,
           perplexity = 10,
           theta = 0.1,
           return_table = FALSE,
           ...) {
    if (!hasArg(mPT)){
      stop("mPT is missing")
    } else if (class(mPT)=="genoMatriXeR"){
      GM <- mPT@matrix$GMat
    } else if (is.matrix(mPT)){
      GM <- mPT
    } else {
      stop("mPT needs to be a genoMatriXeR object or a numeric matrix")
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

    if (main=="") {
      main <- deparse(substitute(mPt))
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
      pdr_df$clust2[pdr_df$clust == sel_clust[i]] <- sel_clust[i]
    }

    if (!is.null(listRS) & emphasize){
      pdr_df$clust<-pdr_df$clust2
      pdr_df_emph <- pdr_df[pdr_df$clust != "none",]
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
    } else if (ellipse) { # ellipse for all clusters
      p <- p + stat_ellipse(type = "t",
                            geom = "polygon",
                            alpha = 0.15)
    }

    if (emphasize & !label_all) { # label all clusters
      p <- p + geom_text_repel(data = pdr_df_emph,
                               size  = label_size,
                               aes(label = Name),
                               max.overlaps=labMaxOverlap,
                               point.padding = 0.5,
                               segment.color = "grey")
    } else {
      p <- p + geom_text_repel(size  = label_size,
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
    if (return_table) {
      tab <- pdr_df[,3:4]
      rownames(tab) <- NULL
      return(tab)
    } else {
      return(p)
    }

  }

