#' The multiStateQTLExperiment class
#'
#' The multiStateQTLExperiment class is designed to represent multi-state QTL
#' data. It inherits from the \linkS4class{RangedSummarizedExperiment} class.
#' In addition, the class supports storage of multi-state adjusted
#' beta and betaSE results (e.g., mash) via \code{\link{applyMashr}} and
#' storage of summary results (e.g., pairwise sharing) via
#' \code{\link{getPairwiseSharing}}.
#'
#' @param ... Arguments passed to the \code{\link{SummarizedExperiment}} constructor to fill the slots of the base class.
#'
#' @details
#' In this class, rows should represent associations (feature:variant pairs)
#' while columns represent states (e.g. cell types). Assays include beta and betaSE.
#' As with any \linkS4class{SummarizedExperiment} derivative,
#' different quantifications (e.g., counts, CPMs, log-expression) can be stored
#' simultaneously in the \code{\link{assays}} slot,
#' and row and column metadata can be attached using \code{\link{rowData}} and
#'  \code{\link{colData}}, respectively.
#'
#' The extra arguments in the constructor (e.g., \code{\link{reducedDims}})
#' represent the main extensions implemented in the SingleCellExperiment class.
#' This enables a consistent, formalized representation of data structures
#' that are commonly encountered during single-cell data analysis.
#' Readers are referred to the specific documentation pages for more details.
#'
#' A MSQE can also be coerced from a \linkS4class{SummarizedExperiment} or
#' \linkS4class{RangedSummarizedExperiment} instance.
#'
#' @return A multiStateQTLExperiment object.
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' nStates <- 10
#' nQTL <- 100
#' b <- matrix(rnorm(nStates * nQTL), ncol=nStates)
#' se <- matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
#'
#' pca <- matrix(runif(nStates*5), nStates)
#'
#' msqe <- multiStateQTLExperiment(assays=list(betas=b, se=se),
#'                                 reducedDims=SimpleList(PCA=pca))
#' msqe
#'
#' ## coercion from SummarizedExperiment
#' se <- SummarizedExperiment(assays=list(betas=b, se=se))
#' as(se, "multiStateQTLExperiment")
#'
#' @docType class
#' @aliases
#' coerce,SummarizedExperiment,multiStateQTLExperiment-method
#' coerce,RangedSummarizedExperiment,multiStateQTLExperiment-method
#' @export
#' @importFrom S4Vectors SimpleList
#' @importFrom methods is as
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'
multiStateQTLExperiment <- function(...,
                                    reducedDims=list()) {

  se <- SummarizedExperiment(...)
  if(!is(se, "RangedSummarizedExperiment")) {
    se <- as(se, "RangedSummarizedExperiment")
  }
  .rse_to_msqe(se,
               reducedDims=reducedDims)
}

#' @importFrom S4Vectors DataFrame SimpleList
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom methods new
#' @importFrom BiocGenerics nrow ncol
.rse_to_msqe <- function(rse,
                         reducedDims=list()) {

  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }

  out <- new("multiStateQTLExperiment",
             rse,
             int_elementMetadata=new("DFrame", nrows=nrow(rse)),
             int_colData=new("DFrame", nrows=ncol(rse)))

  reducedDims(out) <- reducedDims

  out
}

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("RangedSummarizedExperiment", "multiStateQTLExperiment", function(from) {
  .rse_to_msqe(from)
})

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("SummarizedExperiment", "multiStateQTLExperiment", function(from) {
  .rse_to_msqe(as(from, "RangedSummarizedExperiment"))
})
