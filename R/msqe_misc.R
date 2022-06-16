#' @title
#' Miscellaneous multiStateQTLExperiment methods
#'
#' @description
#' Miscellaneous methods for the \linkS4class{multiStateQTLExperiment} class
#' that do not fit in any other documentation category.
#'
#' @section Available methods:
#' In the following code snippets, \code{x} and \code{object} are \linkS4class{multiStateQTLExperiment} objects.
#' \describe{
#' \item{\code{show(object)}:}{Print a message to screen describing the contents of \code{object}.}
#' \item{\code{objectVersion(x)}:}{Return the version of the package with which \code{x} was constructed.}
#' }
#'
#' @author Christina Azodi
#' @seealso
#' \code{\link{updateObject}}, where \code{objectVersion} is used.
#'
#' @examples
#' msqe <- mockMSQE()
#' objectVersion(msqe)
#'
#' @name SCE-miscellaneous
#' @rdname miscellaneous
#' @docType methods
#' @aliases
#' objectVersion
#' objectVersion,multiStateQTLExperiment-method
NULL

#' @export
setMethod("objectVersion", "multiStateQTLExperiment", function(x) {
  int_metadata(x)$version
})


