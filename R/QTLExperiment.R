#' @title The QTLExperiment class
#'
#' @description
#'
#' The QTLExperiment class is designed to represent multi-state QTL
#' data. It inherits from the \linkS4class{RangedSummarizedExperiment} class.
#' In addition, the class supports storage of multi-state adjusted
#' beta and betaSE results (e.g., mash) and
#' storage of summary results (e.g., pairwise sharing).
#'
#' @param ... Arguments passed to the \code{\link{SummarizedExperiment}}
#' constructor to fill the slots of the base class.
#' @param state_id An array of state IDs the length of ncol(QTLe).
#' @param feature_id An array of feature IDs the length of nrow(QTLe).
#' @param variant_id An array of variant IDs the length of nrow(QTLe).
#'
#' @details
#' In this class, rows should represent associations (feature_id:variant_id pairs)
#' while columns represent states (e.g. tissues). Assays include betas and
#' error associated with the betas (e.g. standard errors).
#' As with any \linkS4class{SummarizedExperiment} derivative,
#' different information (e.g., test-statistics, significance calls) can be
#' stored in user defined \code{\link{assay}} slots, and additional row and
#' column metadata can be attached using \code{\link{rowData}} and
#' \code{\link{colData}}, respectively.
#'
#' The extra arguments in the constructor (\code{\link{feature_id}},
#' \code{\link{variant_id}}, and \code{\link{state_id}})
#' represent the main extensions implemented in the QTLExperiment
#' class. This enables a consistent, formalized representation of key aspects
#' of multi-state QTL data that are universal to the data structure.
#' that are commonly encountered during single-cell data analysis.
#' Readers are referred to the specific documentation pages for more details.
#'
#' A QTLe can also be coerced from a \linkS4class{SummarizedExperiment} or
#' \linkS4class{RangedSummarizedExperiment} instance.
#'
#' @return A QTLExperiment object.
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' nStates <- 10
#' nQTL <- 100
#' betas <- matrix(rnorm(nStates * nQTL), ncol=nStates)
#' error <- matrix(abs(rnorm(nStates * nQTL)), ncol=nStates)
#'
#' qtle <- QTLExperiment(assays=list(betas=betas, error=error),
#'                                 feature_id = sample(1:10, nQTL, replace=TRUE),
#'                                 variant_id = sample(seq(1e3:1e5), nQTL))
#' qtle
#'
#' ## coercion from SummarizedExperiment
#' se <- SummarizedExperiment(assays=list(betas=betas, error=error),
#'                            feature_id = sample(1:10, nQTL, replace=TRUE),
#'                            variant_id = sample(seq(1e3:1e5), nQTL))
#' as(se, "QTLExperiment")
#'
#' @docType class
#'
#' @aliases
#' coerce,SummarizedExperiment,QTLExperiment-method
#' coerce,RangedSummarizedExperiment,QTLExperiment-method
#'
#' @name QTLExperiment
NULL

.feat_field <- "feature_id"
.var_field <- "variant_id"
.state_field <- "state_id"


#' @export
#' @importFrom S4Vectors SimpleList
#' @importFrom methods is as callNextMethod coerce
#' @importFrom SummarizedExperiment SummarizedExperiment rowData colData assays
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
QTLExperiment <- function(..., state_id=NULL, feature_id=NULL,
                                    variant_id=NULL) {

  se <- SummarizedExperiment(...)
  if(!is(se, "RangedSummarizedExperiment")) {
    se <- as(se, "RangedSummarizedExperiment")
  }

  if(is.null(state_id)){
    state_id <- colnames(se)
  }

  if(is.null(feature_id)){
    feature_id <- rowData(se)[[.feat_field]]
  }

  if(is.null(variant_id)){
    variant_id <- rowData(se)[[.var_field]]
  }

  .rse_to_qtle(se, state_id, feature_id, variant_id)
}

#' @importFrom checkmate checkInt checkIntegerish checkNumber checkNumeric
#' @importFrom checkmate checkLogical
#' checkFlag
setValidity("QTLExperiment", function(object) {

  row_data_names <- names(int_rowData(object))
  assay_names <- names(assays(object))

  checks <- c(betas = ifelse("betas" %in% assay_names,
                              TRUE, "assay needed"),
              error = ifelse("error" %in% assay_names,
                             TRUE, "assay needed")) #,
              #feature_id = ifelse("feature_id" %in% row_data_names,
              #                    TRUE, "needed in rowData"),
              #variant_id = ifelse("variant_id" %in% row_data_names,
              #                    TRUE, "needed in rowData"))


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

.rse_to_qtle <- function(rse, state_id, feature_id, variant_id) {

  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }

  colData = DataFrame(state_id)
  names(colData) <- .state_field

  rowData = DataFrame(feature_id, variant_id)
  names(rowData) <- c(.feat_field, .var_field)

  out <- new("QTLExperiment", rse,
             int_colData = colData,
             int_rowData = rowData)
  out <- .sync_qtle_ids(out)
  out

}

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("RangedSummarizedExperiment", "QTLExperiment", function(from) {

    if(any(length(colnames(from)) != ncol(from),
           ! .feat_field %in% names(rowData(from)),
           ! .var_field %in% names(rowData(from)))){
      warning("state_ids need to be provided in the colnames and
      feature_ids and variant_ids need to be provided in the rowData")
    }

    .rse_to_qtle(from)
})

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("SummarizedExperiment", "QTLExperiment", function(from) {

  if(any(length(colnames(from)) != ncol(from),
         ! .feat_field %in% names(rowData(from)),
         ! .var_field %in% names(rowData(from)))){
      warning("state_ids need to be provided in the colnames and
      feature_ids and variant_ids need to be provided in the rowData")
  }

  .rse_to_qtle(as(from, "RangedSummarizedExperiment"))
})


#' @importFrom SummarizedExperiment rowData
.sync_qtle_ids <- function(x){

  rowData(x)[[.var_field]]  <- int_rowData(x)[[.var_field]]
  rowData(x)[[.feat_field]]  <- int_rowData(x)[[.feat_field]]

  row.names(x) <- paste(rowData(x)[[.feat_field]],
                        rowData(x)[[.var_field]], sep="|")

  x
}

