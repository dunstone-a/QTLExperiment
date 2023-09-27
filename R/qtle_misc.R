#' @title
#' Miscellaneous methods to apply to a
#' \linkS4class{QTLExperiment} object.
#'
#' @description
#' Miscellaneous methods for the \linkS4class{QTLExperiment} class.
#'
#' @param x A \linkS4class{QTLExperiment} object.
#' @param value Any character-like object or \code{NULL} to
#'              remove existing labels.
#' @param ... Additional arguments.
#'
#' @section Available methods:
#' In the following code snippets, \code{x} is a
#' \linkS4class{QTLExperiment} objects.
#'
#' \describe{
#' \item{\code{objectVersion(x)}:}{Return the version of the package with which
#'                                 \code{x} was constructed.}
#' \item{\code{mainExpName(x)}:}{Return the name assigned to the \code{x}.}
#' }
#'
#' @author Christina B. Azodi
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition
#' and \code{\link{updateObject}} when \code{objectVersion} is used.
#'
#' @examples
#' qtle <- mockQTLE()
#' objectVersion(qtle)
#' mainExpName(qtle)
#' mainExpName(qtle) <- "test_name"
#' mainExpName(qtle)
#'
#' @name QTLe-miscellaneous
#' @rdname miscellaneous
#' @docType methods
#' @aliases
#' objectVersion
#' objectVersion,QTLExperiment-method
#' mainExpName
#' mainExpName
#' mainExpName,QTLExperiment-method
#' mainExpName<-
#' mainExpName<-,QTLExperiment,character_OR_NULL-method
#'
NULL

#' @export
setMethod("objectVersion", "QTLExperiment", function(x) {
    int_metadata(x)$version
})

#' @export
setMethod("mainExpName", "QTLExperiment", function(x) {
    int_metadata(x)$mainExpName
})

#' @export
setReplaceMethod("mainExpName", c("QTLExperiment",
                                  "character_OR_NULL"), function(x, value) {
                                      int_metadata(x)$mainExpName <- value
                                      x
                                  })


