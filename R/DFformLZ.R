#' Dataframe from MultiLocalZScore object
#'
#' @return a data.frame from [multiLocalZScore-class]
#'
#' @usage DFfromLZ( mLZ, RS )
#' @param mLZ object class MultilocalZScore
#' @param RS string name of single region set present in the mLZ object
#'
#' @keywords internal

DFfromLZ <- function(mLZ, RS) {

  nelem <- grep(paste0("\\b",RS,"\\b"), names(mlzsMultiLocalZscores(mLZ)$shifed_ZSs))

  lineLZ <- mlzsMultiLocalZscores(mLZ)$shifed_ZSs[[nelem]]
  nreg <- mlzsParameters(mLZ)$Nregions

  zs <- mlzsMultiLocalZscores(mLZ)$resumeTab$z_score[nelem]
  pv <- mlzsMultiLocalZscores(mLZ)$resumeTab$adj.p_value[nelem]
  mlzsMultiLocalZscores(mLZ)$resumeTab$n_overlaps == nreg

  df <-
    data.frame(
      name = rep(RS, length(lineLZ)),
      lzscore = lineLZ,
      normLocalZscore = lineLZ / (sqrt(nreg)),
      shift = mlzsMultiLocalZscores(mLZ)$shifts
    )

  return(df)

}
