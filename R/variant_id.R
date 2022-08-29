#' Variant ID methods
#'
#' Gets or sets the variant_id for all tests in a \linkS4class{QTLExperiment} object.
#'
#' @param object A \linkS4class{QTLExperiment} object.
#' @param value A numeric vector of length equal to \code{nrow(object)},
#'              containing the name of the variant (e.g. SNP) being tested.
#' @param ... Additional arguments, currently ignored.
#' @param onAbsence String indicating an additional action to take when
#'                  variant_ids are absent: nothing (\code{"none"}), a warning
#'                  (\code{"warn"}) or an error (\code{"error"}).
#'
#' @details
#' QTL are associations between a genetic variants and a quantitative feature.
#' The \code{variant_id} methods can be used to get or set variant IDs for all
#' tests in a \linkS4class{QTLExperiment} object. The values are
#' stored in the \code{\link{rowData}} and in the \code{\link{int_rowData}} as
#' the \code{\link{variant_id}} field so it can be easily accessed but not
#' accidentally removed or overwritten.
#'
#' @return
#' For \code{variant_id}, a vector is returned containing the name of the
#' variant tested in each association.
#' For \code{variant_id<-}, a modified \code{object} is returned with the
#' updated variant_ids in \code{\link{rowData}}, \code{\link{int_rowData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#'
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition.
#'
#' @author Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLe()
#' variant_id(qtle) <- sample(paste0("rsid", 1:100), nrow(qtle), replace=TRUE)
#' variant_id(qtle)
#'
#' @name variant_id
NULL

.var_field <- "variant_id"

#' @export
#' @rdname variant_id
#' @importFrom SummarizedExperiment rowData
#'
setMethod("variant_id", "QTLExperiment",
          function(object, onAbsence="none") {

            object <- updateObject(object)
            output <- rowData(object)[[.var_field]]
            .absent_action(object, val=output, fun="variant_id", onAbsence=onAbsence)
            output

          })


#' @export
#' @rdname variant_id
#' @importFrom SummarizedExperiment rowData<- rowData
setReplaceMethod("variant_id", "QTLExperiment",
                 function(object, ..., value) {

                   object <- updateObject(object)
                   int_rowData(object)[[.var_field]] <- value
                   object <- .sync_qtle_ids(object)
                   object

                 })
