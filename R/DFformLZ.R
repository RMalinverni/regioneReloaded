#' Dataframe from MultiLocalZScore object
#'
#' @return a data.frame from \code{\link{mLZS_class}}
#'
#' @usage DFfromLZ( mLZ, RS )
#' @param mLZ object class MultilocalZScore
#' @param RS string name of single region set present in the mLZ object
#'
#' @keywords internal function

DFfromLZ <- function(mLZ, RS) {

  nelem <- grep(paste0("\\b",RS,"\\b"), names(mLZ@multiLocalZscores$shifed_ZSs))

  lineLZ <- mLZ@multiLocalZscores$shifed_ZSs[[nelem]]
  nreg <- mLZ@parameters$Nregions

  zs <- mLZ@multiLocalZscores$resumeTab$z_score[nelem]
  pv <- mLZ@multiLocalZscores$resumeTab$adj.p_value[nelem]
  mLZ@multiLocalZscores$resumeTab$n_overlaps == nreg

  df <-
    data.frame(
      name = rep(RS, length(lineLZ)),
      lzscore = lineLZ,
      normLocalZscore = lineLZ / (sqrt(nreg)),
      shift = mLZ@multiLocalZscores$shifts
    )

  return(df)

}
