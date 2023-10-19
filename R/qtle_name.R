#' @title
#' Return the name of a \linkS4class{QTLExperiment} object.
#'
#' @description
#' Returns the name of an object of class \linkS4class{QTLExperiment}.
#'
#' @param x A \linkS4class{QTLExperiment} object.
#' @param value Any character-like object or \code{NULL} to
#'              remove existing labels.

#' @section Available methods:
#' In the following code snippets, \code{x} is a
#' \linkS4class{QTLExperiment} objects.
#'
#' \describe{
#' \item{\code{mainExpName(x)}:}{Return the name assigned to \code{x}.}
#' \item{\code{mainExpName(x) <- value:}}{Change the name assigned to \code{x} 
#' to \code{value}.}
#' \item{\code{mainExpName(x) <- NULL:}}{Remove the name associated to \code{x}.}
#' }
#'
#' @returns For \code{mainExpName(x)}, returns the name associated to \code{x}.
#' 
#' For \code{mainExpName(x) <- value}, the name of the object \code{x} is updated.
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
setMethod("mainExpName", "QTLExperiment", function(x) {
    int_metadata(x)$mainExpName
})

#' @export
setReplaceMethod(
    "mainExpName", c("QTLExperiment", "character_OR_NULL"), function(x, value) {
        int_metadata(x)$mainExpName <- value
        x
})


