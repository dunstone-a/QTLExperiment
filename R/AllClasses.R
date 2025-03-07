#' An S4 class to represent QTL summary statistics.
#'
#' @slot elementMetadata A DataFrame containing at minimum feature_id and variant_id information. This is accessed using rowData. 
#' @slot colData A DataFrame containing at minimum state_id information.
#' @slot int_metadata A list of additional metadata items to store.
#'
#' @export
#' @rdname QTLExperiment
#' @importFrom utils packageVersion
#' @importFrom S4Vectors SimpleList
#' @importFrom stats setNames
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom S4Vectors DataFrame SimpleList
#'
setClass("QTLExperiment",
    slots=c(
        elementMetadata="DataFrame",
        colData="DataFrame",
        int_metadata="list"),
    contains="RangedSummarizedExperiment",
    prototype=prototype(
        int_metadata=list(
            version=packageVersion("QTLExperiment"),
            mainExpName=NULL))
)
