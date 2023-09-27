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
#' In this class, rows should represent associations (feature_id:variant_id
#' pairs) while columns represent states (e.g. tissues). Assays include betas
#' and error associated with the betas (e.g. standard errors).
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
#' qtle <- QTLExperiment(assays=list(betas=betas, errors=error),
#'                       feature_id = sample(1:10, nQTL, replace=TRUE),
#'                       variant_id = sample(seq(1e3:1e5), nQTL),
#'                       state_id = LETTERS[1:nStates])
#' qtle
#'
#' ## coercion from SummarizedExperiment
#' mock_sumstats <- mockSummaryStats(nStates=10, nQTL=100)
#' se <- SummarizedExperiment(assays=list(betas=mock_sumstats$betas,
#'                                        errors=mock_sumstats$error))
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
QTLExperiment <- function(..., state_id=NULL, feature_id=NULL, variant_id=NULL){

    rse <- SummarizedExperiment(...)

    if(!is(rse, "RangedSummarizedExperiment")) {
        rse <- as(rse, "RangedSummarizedExperiment")
    }

    if(is.null(state_id)){
        rse <- .checkSEcolOrder(rse)
        state_id <- colnames(rse)
    }

    if(is.null(feature_id) | is.null(variant_id)){
        rse <- .checkSErowOrder(rse)
        test_id <- row.names(rse)
        feature_id <- gsub("\\|.*", "", test_id)
        variant_id <- gsub(".*\\|", "", test_id)
    }

    .rse_to_qtle(rse, state_id, feature_id, variant_id)
}

#' @importFrom checkmate checkInt checkIntegerish checkNumber checkNumeric
#' @importFrom checkmate checkLogical checkFlag
#'
setValidity("QTLExperiment", function(object) {

    row_data_names <- names(int_rowData(object))
    assay_names <- names(assays(object))
    x.rownames <- rownames(object)
    x.colnames <- colnames(object)

    checks <- c(betas = ifelse("betas" %in% assay_names,
                               TRUE, "assay needed"),
                errors = ifelse("errors" %in% assay_names,
                                TRUE, "assay needed"),
                test_ids = ifelse(any(duplicated(x.rownames)),
                                  "duplicate feature|variant rows", TRUE),
                colnames = ifelse(any(duplicated(x.colnames)),
                                  "duplicate state_id columns", TRUE))


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
#'
.rse_to_qtle <- function(rse, state_id, feature_id, variant_id) {

    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }

    colData <- DataFrame(state_id)
    names(colData) <- paste0(".", .state_field)

    rowData <- DataFrame(feature_id, variant_id)
    names(rowData) <- paste0(".", c(.feat_field, .var_field))

    out <- new("QTLExperiment", rse,
               int_colData = colData,
               int_rowData = rowData)
    out <- recover_qtle_ids(out)
    out

}

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("RangedSummarizedExperiment", "QTLExperiment", function(from) {

    if(! all(grepl("|", rownames(from), fixed=TRUE))) {
        warning("rse rownames must be in the format feature_id|variant_id...")
    }

    state_id <- colnames(from)
    feature_id <- gsub("\\|.*", "", rownames(from))
    variant_id <- gsub(".*\\|", "", rownames(from))

    .rse_to_qtle(from, state_id = state_id, feature_id = feature_id,
                 variant_id = variant_id)
})

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("SummarizedExperiment", "QTLExperiment", function(from) {

    if(! all(grepl("|", rownames(from), fixed=TRUE))) {
        warning("se rownames must be in the format feature_id|variant_id...")
    }

    state_id <- colnames(from)
    feature_id <- gsub("\\|.*", "", rownames(from))
    variant_id <- gsub(".*\\|", "", rownames(from))

    .rse_to_qtle(as(from, "RangedSummarizedExperiment"),
                 state_id = state_id, feature_id = feature_id,
                 variant_id = variant_id)
})


