#' Feature ID methods
#'
#' Gets or sets the feature_id for all tests in a \linkS4class{QTLExperiment} object.
#'
#' @param object A \linkS4class{QTLExperiment} object.
#' @param value A numeric vector of length equal to \code{nrow(object)},
#'              containing the name of the feature (e.g. gene) being tested.
#' @param ... Additional arguments, currently ignored.
#' @param onAbsence String indicating an additional action to take when size factors are absent:
#' nothing (\code{"none"}), a warning (\code{"warn"}) or an error (\code{"error"}).
#'
#' @details
#' QTL are associations between a genetic variant and a quantitative feature.
#' The \code{feature_id} methods can be used to get or set feature IDs for all
#' tests in a \linkS4class{QTLExperiment} object. The values are
#' stored in the \code{\link{rowData}} and in the \code{\link{int_rowData}} as
#' the \code{\link{feature_id}} field so it can be easily accessed but not
#' accidentally removed or overwritten.
#'
#' @return
#' For \code{feature_id}, a vector is returned containing the name of the
#' feature tested in each association.
#' For \code{feature_id<-}, a modified \code{object} is returned with the
#' updated feature_ids in \code{\link{rowData}}, \code{\link{int_rowData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#'
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition.
#'
#' @author Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLe()
#' feature_id(qtle) <- sample(LETTERS, nrow(qtle), replace=TRUE)
#' feature_id(qtle)
#'
#' @name feature_id
NULL

.feat_field <- "feature_id"

#' @export
#' @rdname feature_id
#' @importFrom SummarizedExperiment rowData
#'
setMethod("feature_id", "QTLExperiment",
          function(object, onAbsence="none") {

  object <- updateObject(object)
  output <- rowData(object)[[.feat_field]]
  .absent_action(object, val=output, fun="feature_id", onAbsence=onAbsence)
  output

})


#' @export
#' @rdname feature_id
#' @importFrom SummarizedExperiment rowData<- rowData
setReplaceMethod("feature_id", "QTLExperiment",
                 function(object, ..., value) {

  object <- updateObject(object)
  int_rowData(object)[[.feat_field]] <- value
  object <- .sync_qtle_ids(object)
  object

})
