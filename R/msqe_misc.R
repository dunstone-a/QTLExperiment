#' @title
#' Miscellaneous multiStateQTLExperiment methods
#'
#' @description
#' Miscellaneous methods for the \linkS4class{multiStateQTLExperiment} class.
#'
#' @param x A \linkS4class{multiStateQTLExperiment} object.
#' @param value For `colLabels()`, any vector-like object of \code{ncol(object)}
#'              length containing labels for all states. Or \code{NULL} to
#'              remove existing labels.
#' @param ... Additional arguments.
#' @param onAbsence For `colLabels()` a string indicating an additional action
#'                  to take when labels are absent: nothing (\code{"none"}),
#'                  a warning (\code{"warn"}) or an error (\code{"error"}).
#'
#' @section Available methods:
#' In the following code snippets, \code{x} and \code{object} are
#' \linkS4class{multiStateQTLExperiment} objects.
#' \describe{
#' \item{\code{show(object)}:}{Print a message to screen describing the
#'                             contents of \code{object}.}
#' \item{\code{objectVersion(x)}:}{Return the version of the package with which
#'                                 \code{x} was constructed.}
#' \item{\code{mainExpName(x)}:}{Return the name assigned to the \code{x}.}
#' \item{\code{colLabels(x)}:}{Return an array with state names from \code{x}.}
#' }
#'
#' @author Christina Azodi
#' @seealso
#' \linkS4class{multiStateQTLExperiment}, for the underlying class definition
#' and \code{\link{updateObject}} when \code{objectVersion} is used.
#'
#' @examples
#' msqe <- mockMSQE()
#' objectVersion(msqe)
#' mainExpName(msqe)
#' mainExpName(msqe) <- "test_name"
#' mainExpName(msqe)
#' colLabels(msqe) <- sample(LETTERS, ncol(msqe), replace=TRUE)
#' colLabels(msqe)
#'
#' @name MSQE-miscellaneous
#' @rdname miscellaneous
#' @docType methods
#' @aliases
#' objectVersion
#' objectVersion,multiStateQTLExperiment-method
#' mainExpName
#' mainExpName,multiStateQTLExperiment-method
#' mainExpName<-
#' mainExpName<-,multiStateQTLExperiment-method
#' colLabels
#' colLabels,multiStateQTLExperiment-method
#'
NULL

#' @export
setMethod("objectVersion", "multiStateQTLExperiment", function(x) {
  int_metadata(x)$version
})


#' @export
setMethod("mainExpName", "multiStateQTLExperiment", function(x) {
  int_metadata(x)$mainExpName
})

#' @export
setReplaceMethod("mainExpName", c("multiStateQTLExperiment",
                                  "character_OR_NULL"), function(x, value) {
  int_metadata(x)$mainExpName <- value
  x
})

.label_field <- "label"

#' @export
#' @importFrom SummarizedExperiment colData
setMethod("colLabels", "multiStateQTLExperiment", function(x, onAbsence="none") {
  output <- colData(x)[[.label_field]]
  .absent_action(x, val=output, fun="colLabels", onAbsence=onAbsence)
  output
})

#' @export
#' @importFrom SummarizedExperiment colData<- colData
setReplaceMethod("colLabels", "multiStateQTLExperiment", function(x, ..., value) {
  colData(x)[[.label_field]] <- value
  x
})

