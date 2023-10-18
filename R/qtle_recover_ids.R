#' @title Recover QTLExperiment IDs
#'
#' @description Function to recover protected rowData (feature_id, variant_id)
#' and colData (state_id) from internal structure.
#'
#' @param object QTLExperiment object
#'
#' @returns A \linkS4class{QTLExperiment} object with recovered rowData or colData.
#'
#' @examples
#'
#' # Recover a column in colData
#'
#' qtle <- mockQTLE()
#'
#' head(colData(qtle))
#'
#' new_colData <- DataFrame(
#'     list(some_info1=LETTERS[1:ncol(qtle)],
#'         some_info2=c(1:ncol(qtle))))
#'
#' # colData is overwritten
#' colData(qtle) <- new_colData
#' head(colData(qtle))
#'
#' # colData is recovered
#' qtle <- recover_qtle_ids(qtle)
#' head(colData(qtle))
#'
#' # Recover information from rowData
#'
#' head(rowData(qtle))
#'
#' # variant_id are shuffled accidentally
#' rowData(qtle)$variant_id <- sample(rowData(qtle)$variant_id, nrow(qtle))
#' head(rowData(qtle))
#'
#' # Recover rowData
#' qtle <- recover_qtle_ids(qtle)
#' head(rowData(qtle))
#'
#'
#' @name QTLe-recover
#' @rdname recover_qtle_ids
#' @docType methods
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom methods validObject
#' @export
#'
#'
recover_qtle_ids <- function(object){

    colData(object)[[.state_field]] <- int_colData(object)[[paste0(".", .state_field)]]
    rowData(object)[[.var_field]] <- int_rowData(object)[[paste0(".", .var_field)]]
    rowData(object)[[.feat_field]] <- int_rowData(object)[[paste0(".", .feat_field)]]

    colnames(object) <- state_id(object)
    row.names(object) <- paste(feature_id(object), variant_id(object), sep="|")

    try(validObject(object, complete=TRUE))

    object
}

