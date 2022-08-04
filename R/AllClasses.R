#' @export
#' @rdname multiStateQTLExperiment
#' @importFrom utils packageVersion
#' @importFrom S4Vectors SimpleList
#' @importFrom stats setNames
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom S4Vectors DataFrame SimpleList
#'
setClass("multiStateQTLExperiment",
         slots=c(int_rowData = "DataFrame",
                 int_colData = "DataFrame",
                 int_metadata = "list"),
         contains = "RangedSummarizedExperiment",
         prototype = prototype(
           int_metadata=list(
             version=packageVersion("multiStateQTLExperiment"),
             mainExpName=NULL
           )
         ))
