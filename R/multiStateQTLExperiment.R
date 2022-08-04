#' The multiStateQTLExperiment class
#'
#' The multiStateQTLExperiment class is designed to represent multi-state QTL
#' data. It inherits from the \linkS4class{RangedSummarizedExperiment} class.
#' In addition, the class supports storage of multi-state adjusted
#' beta and betaSE results (e.g., mash) and
#' storage of summary results (e.g., pairwise sharing).
#'
#' @param ... Arguments passed to the \code{\link{SummarizedExperiment}}
#' constructor to fill the slots of the base class.
#' @param reducedDims A list of matrix-like objects containing dimensionality
#' reduction results, each of which should have the same number of rows as the
#' output multiStateQTLExperiment object.
#'
#' @details
#' In this class, rows should represent associations (feature_id:variant_id pairs)
#' while columns represent states (e.g. cell types). Assays include betas and
#' error associated with the betas (e.g. standard errors).
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
#' betas <- matrix(rnorm(nStates * nQTL), ncol=nStates)
#' error <- matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
#' rowdata <- DataFrame(feature_id = sample(1:10, nQTL, replace=TRUE),
#'                      variant_id = sample(seq(1e3:1e5), nQTL))
#'
#' msqe <- multiStateQTLExperiment(assays=list(betas=betas, error=error),
#'                                 rowData=rowdata)
#' msqe
#'
#' ## coercion from SummarizedExperiment
#' se <- SummarizedExperiment(assays=list(betas=betas, error=error),
#'                            rowData=rowdata)
#' as(se, "multiStateQTLExperiment")
#'
#' @docType class
#'
#' @aliases
#' coerce,SummarizedExperiment,multiStateQTLExperiment-method
#' coerce,RangedSummarizedExperiment,multiStateQTLExperiment-method
#' @export

#' @importFrom S4Vectors SimpleList
#' @importFrom methods is as callNextMethod coerce
#' @importFrom SummarizedExperiment SummarizedExperiment rowData colData assays
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'
multiStateQTLExperiment <- function(..., reducedDims=list()) {

  se <- SummarizedExperiment(...)
  if(!is(se, "RangedSummarizedExperiment")) {
    se <- as(se, "RangedSummarizedExperiment")
  }
  .rse_to_msqe(se, reducedDims=reducedDims)
}


#' @importFrom checkmate checkInt checkIntegerish checkNumber checkNumeric
#' @importFrom checkmate checkLogical
#' checkFlag
setValidity("multiStateQTLExperiment", function(object) {

  row_data_names <- names(rowData(object))
  assay_names <- names(assays(object))

  checks <- c(betas = ifelse("betas" %in% assay_names,
                              TRUE, "assay needed"),
              error = ifelse("error" %in% assay_names,
                             TRUE, "assay needed"),
              feature_id = ifelse("feature_id" %in% row_data_names,
                                  TRUE, "needed in rowData"),
              variant_id = ifelse("variant_id" %in% row_data_names,
                                  TRUE, "needed in rowData"))


  if (all(checks == TRUE)) {

    if (all(checks == TRUE)) {
      valid <- TRUE
    } else {
      valid <- checks[checks != TRUE]
      valid <- paste(names(valid), valid, sep = ": ")
    }


  } else{
    valid <- checks[checks != TRUE]
    valid <- paste(names(valid), valid, sep = ": ")
  }

  return(valid)
})



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
             int_rowData=new("DFrame", nrows=nrow(rse)),
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



