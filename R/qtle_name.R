#' @title
#' Return the name of a \linkS4class{QTLExperiment} object.
#'
#' @description
#' Returns the name of an object of class \linkS4class{QTLExperiment}.
#'
#' @param object A \linkS4class{QTLExperiment} object.
#' @param value Any character-like object or \code{NULL} to
#'              remove existing labels.

#' @section Available methods:
#' In the following code snippets, \code{object} is a
#' \linkS4class{QTLExperiment} objects.
#'
#' \describe{
#' \item{\code{mainExpName(object)}:}{Return the name assigned to \code{object}.}
#' \item{\code{mainExpName(object) <- value:}}{Change the name assigned to \code{object} 
#' to \code{value}.}
#' \item{\code{mainExpName(object) <- NULL:}}{Remove the name associated to \code{object}.}
#' }
#'
#' @returns For \code{mainExpName(object)}, returns the name associated to \code{object}.
#' 
#' For \code{mainExpName(object) <- value}, the name of the object \code{object} is updated.
#' 
#' @author Christina B. Azodi
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition.
#'
#' @examples
#' qtle <- mockQTLE()
#' mainExpName(qtle)
#' mainExpName(qtle) <- "test_name"
#' mainExpName(qtle)
#'
#' @name QTLe-name
#' @rdname name
#' @docType methods
#' @aliases
#' mainExpName
#' mainExpName
#' mainExpName,QTLExperiment-method
#' mainExpName<-
#' mainExpName<-,QTLExperiment,character_OR_NULL-method
#'
NULL

#' @export
setMethod("mainExpName", "QTLExperiment", function(object) {
    int_metadata(object)$mainExpName
})

#' @export
setReplaceMethod(
    "mainExpName", c("QTLExperiment", "character_OR_NULL"), function(object, value) {
        int_metadata(object)$mainExpName <- value
        object
})


