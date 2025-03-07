#' @title
#' Return the version of a \linkS4class{QTLExperiment} object
#'
#' @description
#' Specifies the version of the \link{QTLExperiment} package that an object of class
#' \linkS4class{QTLExperiment} was created with.
#'
#' @param object A \linkS4class{QTLExperiment} object.
#'
#' @section Available methods:
#' In the following code snippets, \code{object} is a
#' \linkS4class{QTLExperiment} objects.
#'
#' \describe{
#' \item{\code{objectVersion(object)}:}{Return the version of the package with which
#'                                 \code{object} was constructed.}
#'  }
#'
#' @author Christina B. Azodi, Amelia Dunstone
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition
#' and \code{\link{updateObject}} to update the object to the latest version.
#'
#' @return
#' A package version, of class \link{package_version}.
#' 
#' @examples
#' qtle <- mockQTLE()
#' objectVersion(qtle)
#'
#' @name QTLe-version
#' @rdname version
#' @docType methods
#' @aliases
#' objectVersion
#' objectVersion,QTLExperiment-method
#'
NULL

#' @export
setMethod("objectVersion", "QTLExperiment", function(object) {
    int_metadata(object)$version
})



