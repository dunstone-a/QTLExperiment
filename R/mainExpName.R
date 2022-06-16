#' Get or set object experiment name
#'
#' Get or set mainExpName for a \linkS4class{multiStateQTLExperiment} instance.
#'
#' @param x A \linkS4class{multiStateQTLExperiment} object.
#' @param value Any character-like object or \code{NULL} to remove existing name.
#'
#' @author Christina Azodi
#'
#' @return
#' For \code{mainExpName}, a character object is returned containing the object
#' name. If no name is available, a \code{NULL} is returned.
#'
#' For \code{mainExpName<-}, a modified \code{x} is returned with a name in
#' \code{\link{mainExpName}}.
#'
#' @seealso
#' \linkS4class{multiStateQTLExperiment}, for the underlying class definition.
#'
#' @examples
#' msqe <- mockMSQE()
#' mainExpName(msqe) <- "demo_experiment"
#' mainExpName(msqe)
#'
#' @docType methods
#' @name mainExpName
#' @aliases mainExpName mainExpName<-
NULL


#' @export
#' @rdname mainExpName
setMethod("mainExpName", "multiStateQTLExperiment", function(x) {
  int_metadata(x)$mainExpName
})


#' @export
#' @rdname mainExpName
setReplaceMethod("mainExpName", c("multiStateQTLExperiment", "character_OR_NULL"), function(x, value) {
  int_metadata(x)$mainExpName <- value
  x
})
