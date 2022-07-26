#' resampleGenome
#'
#'
#' function for use resample starting from a binning of genome
#'
#' @usage resampleGenome(A, simple = FALSE, per.chromosome = FALSE, genome="hg19", ...)
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
#' @importFrom GenomeInfoDb seqlevels
#' @importFrom GenomeInfoDb seqnames
#' @importFrom GenomicRanges width
#' @importFrom GenomicRanges tile
#' @importFrom GenomicRanges resize
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
    mwidth<-round(mean(GenomicRanges::width(A)))
    universe<-unlist(GenomicRanges::tile(getGenome(genome),width = mwidth))
    universe<-GenomicRanges::resize(universe,width=1,fix="center",use.names = FALSE)
  #}
  #######
  #universe <- toGRanges(universe)

  if (per.chromosome==TRUE) {

    chrResample <- function(chr) {
      Achr <- A[GenomeInfoDb::seqnames(A) == chr]
      universe.chr <- universe[GenomeInfoDb::seqnames(universe) == chr]
      resample.chr <- universe.chr[sample(seq_along(universe.chr),
                                          length(Achr))]
      return(resample.chr)
    }

    chr.resampled <- lapply(as.list(GenomeInfoDb::seqlevels(A)), chrResample)
    resampled <- do.call(c, chr.resampled)
  } else {

    resampled <- universe[sample(seq_along(universe), length(A))]
  }
  #####added
  if (univOpt & simple==FALSE){

    resampled <- GenomicRanges::resize(resampled,width=GenomicRanges::width(A),
                                       fix="center",use.names = FALSE)

  }
  ############
  return(resampled)
}
