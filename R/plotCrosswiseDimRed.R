#' plotCrosswiseDimRed
#'
#' @description
#'
#' Plot a visualization of a genomeMatriXeR object (or matrix) using different
#' dimensional reduction algorithms (PCA, tSNE and UMAP).
#'
#' @details
#'
#' This function generates a plot with a two-dimensional representation of the
#' association data stored in a genomeMatriXeR by using either PCA, tSNE or UMAP
#' transformations of the data. This function incorporates a clustering step and
#' allows to highlight specific region sets of interest and the clusters they
#' belong to. In addition to generating a plot, a table with the cluster
#' assignments can be retrieved by setting return_table as TRUE.
#'
#' @usage plotCrosswiseDimRed(mPT, type = "PCA", GM_clust = NA, nc = 5, listRS = NULL, main = "", labSize = 2, emphasize = FALSE,
#' labAll = FALSE, labMaxOverlap = 100, ellipse = TRUE, perplexity = 10, theta = 0.1, return_table = FALSE)
#'
#'
#' @param mPT an object of class genoMatriXeR or a numeric matrix.
#' @param type character, Dimensional Reduction algorithm to use ("PCA", "tSNE", "UMAP"). (default  = "PCA")
#' @param GM_clust numeric, vector of clusters used to clusterize the matrix, if NA will the matrix will be clusterized using the \code{\link{kmeans}} function. (default = NA)
#' @param nc numeric, number of cluster to define if using the default kmeans method. (default = 5)
#' @param listRS list of vector, a list of names of regionset of interest to be highlighted in the graph. (default = NULL)
#' @param main character, title for the plot. (default = "")
#' @param labSize numeric, size for point labels in the plot, if 0 no labels will be plotted (default = 2)
#' @param emphasize logical, if listRS is not NULL and emphasize is TRUE only the cluster in which the elements of listRS are present will be highlighted. (default = FALSE)
#' @param labAll logical, if TRUE data points which are not in listRS when emphasize = TRUE are labelled. (default = FALSE)
#' @param labMaxOverlap numeric, max.overlaps for \code{\link{geom_text_repel}}. (default = 100)
#' @param ellipse logical, if TRUE ellipses will be drawn around the clusters. (default = FALSE)
#' @param perplexity numeric, if type = "tSNE" value of perplexity for the function \code{\link{Rtsne}}. (default = 10)
#' @param theta numeric, if type = "tSNE" value of theta for the function \code{\link{Rtsne}}. (default = 0.1)
#' @param return_table logical, if TRUE a table with the cluster assigned to each region is returned instead of the plot. (default = FALSE)
#'
#' @return A ggplot object or a table with cluster assignments is returned.
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
#' CDR_clust <- plotCrosswiseDimRed(cw_Alien_ReG, type = "UMAP",return_table = TRUE)
#'
#' print(CDR_clust)
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
           labSize = 2,
           emphasize = FALSE,
           labAll = FALSE,
           labMaxOverlap = 100,
           ellipse = TRUE,
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
      GM_clust <- stats::kmeans(GM, centers = nc)

    }

    df <- df1 <- data.frame()
    vec <- vec2 <- vec3 <- vector()

    for ( i in 1:nc){

      nms <- names(GM_clust$cluster[GM_clust$cluster == i])
      GMmn <- mean(GM[nms,nms])
      GMsd <- sd(GM[nms,nms])
      vec[i]<-GMmn
      vec2[i]<-GMsd
      df <- data.frame(Name=nms,
                      Cluster = rep(paste0("clust_",i),length(nms)),
                      Mean = GMmn,
                      SD = GMsd,
                      Perc = round(GMsd/GMmn, digits = 2)
                      )

      df1<-rbind(df1, df)
    }


    if (type == "PCA") {

      pdr_out = stats::princomp(GM, scores = TRUE)
      pdr_df = data.frame(pdr_out$scores)
      pdr_df <- pdr_df[, c("Comp.1", "Comp.2")]
      colnames(pdr_df) <- c("x", "y")
      pdr_df$Name <- rownames(GM)

    }

    if (type == "tSNE") {

      pdr_out <- Rtsne::Rtsne(GM, perplexity = perplexity, theta = theta, check_duplicates = FALSE)
      pdr_df <- data.frame(x = pdr_out$Y[, 1],
                   y = pdr_out$Y[, 2],
                   Name = rownames(GM))

    }

    if (type == "UMAP"){
      pdr_out <- umap::umap(GM)
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
      ggplot2::ggplot(pdr_df, aes(
        x = x,
        y = y,
        label = Name,
        color = clust
      )) +
      ggplot2::geom_point()

    if(emphasize & ellipse){ # ellipse only for emphasized clusters
      p <- p + ggplot2::stat_ellipse(data = pdr_df_emph,
                            type = "t",
                            geom = "polygon",
                            alpha = 0.15)
    } else if (ellipse) { # ellipse for all clusters
      p <- p + ggplot2::stat_ellipse(type = "t",
                            geom = "polygon",
                            alpha = 0.15)
    }

    if (emphasize & !labAll) { # label all clusters
      p <- p + ggrepel::geom_text_repel(data = pdr_df_emph,
                               size  = labSize,
                               aes(label = Name),
                               max.overlaps=labMaxOverlap,
                               point.padding = 0.5,
                               segment.color = "grey")
    } else {
      p <- p + ggrepel::geom_text_repel(size  = labSize,
                               aes(label = Name),
                               max.overlaps=labMaxOverlap,
                               point.padding = 0.5,
                               segment.color = "grey")
    }

    if(type=="PCA"){
      p <- p + ggplot2::labs(title = "PCA plot" ,
                    subtitle = main
                   )
    }

    if(type=="tSNE"){
      p <- p + ggplot2::labs(title = "tSNE plot" ,
        subtitle = main,
        caption = paste0("perplexity: ", perplexity, " theta: ", theta))
    }

    if(type=="UMAP"){
      p <- p + ggplot2::labs(title = "UMAP plot" ,
                    subtitle = main)
    }
    if (return_table) {
      #tab <- pdr_df[,3:4]
      #rownames(tab) <- NULL
      p
      tab <- df1
      return(tab)
    } else {
      return(p)
    }

  }

