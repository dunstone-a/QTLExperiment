#' @title
#' Named colData getters and setters
#'
#' @description
#' These are methods for getting or setting protected colData columns (i.e.
#' state_id).
#'
#' @section Available methods:
#' Here \code{x} is a \linkS4class{QTLExperiment} object,
#' \code{value} is a matrix-like object with the same dimensions as \code{x},
#' and \code{...} are further arguments passed to \code{\link{state_id}}
#' (for the getter) or \code{\link{state_id<-}} (for the setter).
#'
#' \describe{
#' \item{\code{state_id(x, ...)}, \code{state_id(x, ...) <- value}:}{
#' Get or set the state (i.e. column) names.
#' }
#' }
#'
#' @details
#' QTL are associations between a genetic variant and a quantitative state.
#' The \code{state_id} methods can be used to get or set state IDs for all
#' tests in a \linkS4class{QTLExperiment} object. The values are
#' stored in the \code{\link{colData}} and in the \code{\link{int_colData}} as
#' the \code{\link{state_id}} field so it can be easily accessed but not
#' accidentally removed or overwritten.
#'
#' @return
#' For \code{state_id}, a vector is returned containing the name of the
#' state tested in each association.
#' For \code{state_id<-}, a modified \code{object} is returned with the
#' updated state_ids in \code{\link{colData}}, \code{\link{int_colData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLE()
#' state_id(qtle) <- sample(LETTERS, ncol(qtle), replace=TRUE)
#' state_id(qtle)
#'
#' @name qtle-col_ids
#' @rdname col_ids
#' @docType methods
#' @aliases
#' state_id
#' state_id<-
#' state_id,QTLExperiment-method
#' state_id<-,QTLExperiment-method
#'
#'
NULL

.state_field <- "state_id"


#' @importFrom SummarizedExperiment colData
#'
GET_COLDATA_FUN <- function(values, ...) {
  (values) # To ensure evaluation
  function(object, ...) {
    output <- colData(object)[[.state_field]]
    output
  }
}

#' @importFrom SummarizedExperiment colData colData<-
#'
SET_COLDATA_FUN <- function(values, ...) {
  (values)
  function(object, ..., value) {
    object <- updateObject(object)
    int_colData(object)[[paste0(".", .state_field)]] <- value
    object <- recover_qtle_ids(object)
    object
  }
}

#' @export
setMethod("state_id", "QTLExperiment", GET_COLDATA_FUN("state_id"))

#' @export
setReplaceMethod("state_id", c("QTLExperiment", "ANY"), SET_COLDATA_FUN("state_id"))

