#' @title
#' Modify and view state ID
#'
#' @description
#' These are methods for getting or setting protected colData columns (i.e.
#' state_id).
#'
#' @details
#' QTL are associations between a genetic variant and a quantitative state.
#' The \code{state_id} methods can be used to get or set state IDs for all
#' tests in a \linkS4class{QTLExperiment} object. The values are
#' stored in the \code{\link{colData}} as
#' the \code{\link{state_id}} field so it can be easily accessed but not
#' accidentally removed or overwritten.
#'
#' @param object is a \linkS4class{QTLExperiment} object
#' @param value is a character vector with length equal to the number of columns/states in \code{object}.

#' @return
#' For \code{state_id}, a vector is returned containing the name of the
#' state tested in each association.
#' For \code{state_id<-}, a modified \code{object} is returned with the
#' updated state_ids in \code{\link{colData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#'
#' @author
#' Christina B Azodi, Amelia Dunstone
#'
#' @examples
#' qtle <- mockQTLE()
#' state_id(qtle) <- gsub("state", "State_", state_id(qtle))
#' state_id(qtle)
#'
#' @name qtle-state-id
#' @rdname state-id
#' @docType methods
#' 
#' @aliases
#' state_id
#' state_id<-
#' state_id,QTLExperiment-method
#' state_id<-,QTLExperiment-method

NULL

.state_field <- "state_id"


#' @importFrom SummarizedExperiment colData
#' @rdname state-id
#' @export
setMethod("state_id", "QTLExperiment", function(object) {
    out <- colData(object)[[.state_field]]
    out
})

#' @importFrom SummarizedExperiment colData
#' @rdname state-id
#' @export
setReplaceMethod("state_id", c("QTLExperiment", "ANY"), function(object, value) {
    colData(object)[[.state_field]] <- value
    # Don't think this is needed because colData() checks that the new states
    # are valid. 
    # validObject(object)
    object
})