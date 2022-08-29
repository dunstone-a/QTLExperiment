#' State ID methods
#'
#' Gets or sets the state_id for all tests in a \linkS4class{QTLExperiment} object.
#'
#' @param object A \linkS4class{QTLExperiment} object.
#' @param value A numeric vector of length equal to \code{ncol(object)},
#'              containing the name of the state (e.g. gene) being tested.
#' @param ... Additional arguments, currently ignored.
#' @param onAbsence String indicating an additional action to take when size factors are absent:
#' nothing (\code{"none"}), a warning (\code{"warn"}) or an error (\code{"error"}).
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
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition.
#'
#' @author Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLe()
#' state_id(qtle) <- sample(LETTERS, ncol(qtle), replace=TRUE)
#' state_id(qtle)
#'
#' @name state_id
NULL

.state_field <- "state_id"

#' @export
#' @rdname state_id
#' @importFrom SummarizedExperiment colData
#'
setMethod("state_id", "QTLExperiment",
          function(object, onAbsence="none") {

            object <- updateObject(object)
            output <- colData(object)[[.state_field]]
            .absent_action(object, val=output, fun="state_id", onAbsence=onAbsence)
            output

          })


#' @export
#' @rdname state_id
#' @importFrom SummarizedExperiment colData<- colData
setReplaceMethod("state_id", "QTLExperiment",
                 function(object, ..., value) {

                   object <- updateObject(object)
                   int_colData(object)[[.state_field]] <- value
                   object <- .sync_qtle_ids(object)
                   object

                 })
