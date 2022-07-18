#' resampleGenome
#'
#'
#' function for use resample starting from a binning of genome
#'
#' @usage exampleGenome(A, simple = FALSE, per.chromosome = TRUE, genome="hg19", ...)
#'
#' @param A an object of class GenomigRanges
#' @param simple logical, if is TRUE the randomization process will not take care about the singular whit for each regions in A. (defalut = FALSE)
#' @param per.chromosome logical, if TRUE the randomization will be perform by chromosome. (default  = TRUE)
#' @param genome character or GenomicRanges, genome using for the randomization
#' @param ... further arguments to be passed to other methods.
#'
#' @return a permTest object
#' @seealso regioneR::permTest
#'
#'
#'
#' @export resampleGenome
#'




resampleGenome<- function (A ,
                           simple=FALSE,
                           per.chromosome = FALSE,
                           genome="hg19",
                           ...)
{
  if (!methods::hasArg(A))
    stop("A is missing")

  if (!is.logical(per.chromosome))
    stop("per.chromosome must be logical")
  A <- toGRanges(A)

  #####added
  #if (universe=="genome"){
    univOpt<-TRUE
    mwidth<-round(mean(width(A)))
    universe<-unlist(tile(getGenome(genome),width = mwidth))
    universe<-resize(universe,width=1,fix="center",use.names = FALSE)
  #}
  #######
  #universe <- toGRanges(universe)

  if (per.chromosome==TRUE) {

    chrResample <- function(chr) {
      Achr <- A[seqnames(A) == chr]
      universe.chr <- universe[seqnames(universe) == chr]
      resample.chr <- universe.chr[sample(1:length(universe.chr),
                                          length(Achr))]
      return(resample.chr)
    }

    chr.resampled <- lapply(as.list(GenomeInfoDb::seqlevels(A)), chrResample)
    resampled <- do.call(c, chr.resampled)
  } else {

    resampled <- universe[sample(1:length(universe), length(A))]
  }
  #####added
  if (univOpt & simple==FALSE){

    resampled <- resize(resampled,width=width(A),fix="center",use.names = FALSE)

  }
  ############
  return(resampled)
}
