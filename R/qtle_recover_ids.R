#' @title Recover QTLExperiment IDs
#'
#' @description Function to recover protected rowData (feature_id, variant_id)
#' and colData (state_id) from internal structure.
#'
#' @param object QTLExperiment object
#'
#' @returns A \linkS4class{QTLExperiment} object with recovered rowData or colData.
#'
#' @name QTLe-recover
#' @importFrom SummarizedExperiment rowData colData
#' @importFrom methods validObject
#'
#'
.recover_qtle_ids <- function(object){

    # colData(object)[[.state_field]] <- int_colData(object)[[paste0(".", .state_field)]]
    # rowData(object)[[.var_field]] <- int_rowData(object)[[paste0(".", .var_field)]]
    # rowData(object)[[.feat_field]] <- int_rowData(object)[[paste0(".", .feat_field)]]

    colnames(object) <- state_id(object)
    row.names(object) <- paste(feature_id(object), variant_id(object), sep="|")

    try(validObject(object, complete=TRUE))

    object
}

