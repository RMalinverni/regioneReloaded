#' plotCrosswiseDimRed
#'
#' @description
#'
#' Plot a visualization of a [genoMatriXeR][genoMatriXeR-class] object (or matrix) using different
#' dimensional reduction algorithms (PCA, tSNE and UMAP).
#'
#' @details
#'
#' This function generates a plot with a two-dimensional representation of the
#' association data stored in a [genoMatriXeR][genoMatriXeR-class] object by
#' using either PCA, tSNE or UMAP transformations of the data. This function
#' incorporates a clustering step and allows to highlight specific region sets
#' of interest and the clusters they belong to. In addition to generating a plot,
#' a table with the cluster assignments can be retrieved.
#'
#' @usage plotCrosswiseDimRed(mPT, type = "PCA", GM_clust = NA, clust_met =
#' "hclust", nc = 5, listRS = NULL, main = "", labSize = 2, emphasize = FALSE,
#' labAll = FALSE, labMaxOverlap = 100, ellipse = TRUE, colPal = NULL,
#' perplexity = 10, theta = 0.1, return_table = FALSE, return_plot = TRUE, ...)
#'
#'
#' @param mPT an object of class genoMatriXeR or a numerical matrix.
#' @param type character, dimensional reduction algorithm to use ("PCA", "tSNE", "UMAP"). (default  = "PCA")
#' @param GM_clust numeric, vector of assigned clusters used to cluster the matrix. If NA, the matrix will be clustered using the method defined by `clust_met`. (default = NA)
#' @param clust_met character, unsupervised cluster strategy used ([hclust], [kmeans] or [pam]). (default = "hclust")
#' @param nc numeric, number of clusters to define if using the default "kmeans" method. (default = 5)
#' @param listRS list, a list of names of region sets of interest to be highlighted in the graph. (default = NULL)
#' @param main character, title for the plot. (default = "")
#' @param labSize numeric, size for point labels in the plot. If 0, no labels will be plotted. (default = 2)
#' @param emphasize logical, if TRUE, only the cluster in which the elements of `listRS` are present will be highlighted. (default = FALSE)
#' @param labAll logical, if TRUE all data points are labelled, even if not in `listRS` when `emphasize` = TRUE. (default = FALSE)
#' @param labMaxOverlap numeric, max.overlaps for \code{\link{geom_text_repel}}. (default = 100)
#' @param ellipse logical, if TRUE ellipses will be drawn around the clusters. (default = FALSE)
#' @param colPal character, colors to use as palette for the plot. If NULL, default colors will be used. (default = NULL)
#' @param perplexity,theta numeric, if `type` = "tSNE" values of perplexity and theta for the function [Rtsne()]. (default = 10)
#' @param return_table logical, if TRUE a table with the cluster assigned to each region is returned. (default = FALSE)
#' @param return_plot logical, if TRUE a plot is returned. (default = TRUE)
#' @param ... further arguments to be passed on to other methods
#'
#' @return A ggplot object or a table with cluster assignments is returned.
#'
#' @seealso [crosswisePermTest()]
#'
#' @examples
#'
#' data("cw_Alien")
#'
#' cw_Alien_ReG <- makeCrosswiseMatrix(cw_Alien_ReG)
#'
#' plotCrosswiseDimRed(cw_Alien_ReG, type = "PCA")
#'
#' CDR_clust <- plotCrosswiseDimRed(cw_Alien_ReG, type = "UMAP", return_table = TRUE)
#'
#' print(CDR_clust)
#'
#' @export plotCrosswiseDimRed
#'
#' @import ggplot2
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats cutree
#' @importFrom stats kmeans
#' @importFrom stats princomp
#' @importFrom cluster pam
#' @importFrom cluster silhouette
#' @importFrom methods is

plotCrosswiseDimRed <-
  function(mPT,
           type = "PCA",
           GM_clust = NA,
           clust_met = "hclust",
           nc = 5,
           listRS = NULL,
           main = "",
           labSize = 2,
           emphasize = FALSE,
           labAll = FALSE,
           labMaxOverlap = 100,
           ellipse = TRUE,
           colPal = NULL,
           perplexity = 10,
           theta = 0.1,
           return_table = FALSE,
           return_plot = TRUE,
           ...) {

    # Check mPT object
    stopifnot("mPT is missing" = methods::hasArg(mPT))
    stopifnot("mPT needs to be a genoMatriXeR object or a numeric matrix" = {
      methods::is(mPT, "genoMatriXeR") | methods::is(mPT, "matrix")
    })
    stopifnot("The matrix slot of mPT is empty, run first makeCrosswiseMatrix()" = !is.null(gmxrMatrix(mPT)[[1]]))

    # Check arguments
    stopifnot("type must be 'PCA', 'tSNE' or 'UMAP'" = type %in% c("PCA", "tSNE", "UMAP"))
    stopifnot("clust_met must be 'hclust', 'kmeans' or 'pam'" = clust_met %in% c("hclust", "kmeans", "pam"))

    if (methods::is(mPT, "genoMatriXeR")) {
      GM <- getMatrix(mPT)
    } else if (is.matrix(mPT)) {
      GM <- mPT
    }

    if(!all(unlist(listRS) %in% names(gmxrMultiOverlaps(mPT)))) {
      warning("One or more elements in listRS do not match region set names in mPT")
    }

    if (is.na(GM_clust)) {
      if (clust_met == "hclust") {
        clust_tab <- stats::cutree(gmxrMatrix(mPT)$FitRow, k = nc)
        clust_tab <- clust_tab[rownames(GM)]
      }

      if (clust_met == "kmeans") {
        GM_clust <- stats::kmeans(GM, centers = nc)
        clust_tab <- GM_clust$cluster
      }

      if (clust_met == "pam") {
        GM_clust <- cluster::pam(GM, k = nc)
        clust_tab <- GM_clust$cluster
      }
    }

    sil <- cluster::silhouette(clust_tab, dist(GM))
    sumSil <- summary(sil)
    vecSil <- sumSil$clus.avg.widths


    df <- df1 <- data.frame()
    vec <- vec2 <- vec3 <- vector()
    df1 <- do.call("rbind", lapply(seq_len(nc), function(i) {
      nms <- names(clust_tab[clust_tab == i])
      df <- data.frame(
        Name = nms,
        Cluster = rep(paste0("clust_", i), length(nms)),
        ASW = rep(vecSil[i], length(nms))
      )
    })
    )

    df1[is.na(df1)] <- 0

    if (type == "PCA") {
      pdr_out <- stats::princomp(GM, scores = TRUE)
      PoV <- round(pdr_out$sdev^2/sum(pdr_out$sdev^2)*100, 2)
      pdr_df <- data.frame(pdr_out$scores)
      pdr_df <- pdr_df[, c("Comp.1", "Comp.2")]
      colnames(pdr_df) <- c("x", "y")
      pdr_df$Name <- rownames(GM)
    }

    if (type == "tSNE") {
      pdr_out <- Rtsne::Rtsne(GM, perplexity = perplexity, theta = theta, check_duplicates = FALSE)
      pdr_df <- data.frame(
        x = pdr_out$Y[, 1],
        y = pdr_out$Y[, 2],
        Name = rownames(GM)
      )
    }

    if (type == "UMAP") {
      pdr_out <- umap::umap(GM)
      pdr_df <-
        data.frame(
          x = pdr_out$layout[, 1],
          y = pdr_out$layout[, 2],
          Name = rownames(GM)
        )
    }

    if (main == "") {
      main <- deparse(substitute(mPT))
    }


    pdr_df$clust <-
      paste0("clust_", as.factor(clust_tab))

    if (!is.null(listRS) & emphasize) {
      anno_clust <- do.call("rbind", mapply(FUN = function(x, y) {
        data.frame("clust_name" = rep(y, length(x)),
                   "rs_name" = x)
      }, listRS, names(listRS), SIMPLIFY = FALSE))

      pdr_df <- merge(pdr_df, anno_clust, by.x = "Name", by.y = "rs_name", all.x = TRUE, all.y = FALSE)
      pdr_df$clust1 <- pdr_df$clust_name
      pdr_df$clust1[is.na(pdr_df$clust1)] <- "none"

      sel_clust <- pdr_df$clust[pdr_df$clust1 != "none"]

      pdr_df$clust2 <- pdr_df$clust
      pdr_df$clust2[!(pdr_df$clust %in% sel_clust)] <- "none"
      pdr_df$clust <- pdr_df$clust2
      pdr_df_emph <- pdr_df[pdr_df$clust != "none", ]
    } else if (emphasize) {
      warning("Emphasize is TRUE but nothing was supplied as listRS, emphasize will be ignored")

      pdr_df_emph <- pdr_df
    }

    p <-
      ggplot2::ggplot(pdr_df, ggplot2::aes_string(
        x = "x",
        y = "y",
        label = "Name",
        color = "clust"
      )) +
      ggplot2::geom_point()

    if (!is.null(listRS) & emphasize & ellipse) { # ellipse only for emphasized clusters
      p <- p + ggplot2::stat_ellipse(
        data = pdr_df_emph,
        type = "t",
        geom = "polygon",
        alpha = 0.15
      )
    } else if (ellipse) { # ellipse for all clusters
      p <- p + ggplot2::stat_ellipse(
        type = "t",
        geom = "polygon",
        alpha = 0.15
      )
    }

    if (emphasize & !labAll) { # label only emphasized clusters

      p <- p + ggrepel::geom_text_repel(
        data = pdr_df_emph,
        size = labSize,
        ggplot2::aes_string(label = "Name"),
        max.overlaps = labMaxOverlap,
        point.padding = 0.5,
        segment.color = "grey"
      )
    } else { # label all clusters
      p <- p + ggrepel::geom_text_repel(
        size = labSize,
        ggplot2::aes_string(label = "Name"),
        max.overlaps = labMaxOverlap,
        point.padding = 0.5,
        segment.color = "grey"
      )
    }

    if (type == "PCA") {
      p <- p + ggplot2::labs(
        title = "PCA plot",
        subtitle = main,
        caption = paste0("clusterization method: ", clust_met),
        x = paste0("PC1: ", PoV[1], "%"),
        y = paste0("PC2: ", PoV[2], "%")
      )
    }

    if (type == "tSNE") {
      p <- p + ggplot2::labs(
        title = "tSNE plot",
        subtitle = main,
        caption = paste0("perplexity: ", perplexity, " theta: ", theta, "\n",
        "clusterization method: ", clust_met),
        x = "tSNE-1",
        y = "tSNE-2"
        )
    }

    if (type == "UMAP") {
      p <- p + ggplot2::labs(
        title = "UMAP plot",
        subtitle = main,
        caption = paste0("clusterization method: ", clust_met),
        x = "UMAP-1",
        y = "UMAP-2"
      )
    }

    # Palette and theme
    pal <- plotPal(colPal)
    p <- p +
      ggplot2::scale_color_manual(values = pal(nc)) +
      ggplot2::scale_fill_manual(values = pal(nc)) +
      mendel_theme()

    if (return_table == TRUE) {
      if (return_plot == TRUE) {
        plot(p)
      }

      return(df1)
    } else {
      if (return_plot == TRUE) {
        return(p)
      }
    }
  }
