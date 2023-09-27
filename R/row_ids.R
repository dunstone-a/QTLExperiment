#' @title
#' Named rowData getters and setters
#'
#' @description
#' These are methods for getting or setting protected rowData columns (i.e.
#' feature_id and variant_id).
#'
#' @section Available methods:
#' Here \code{x} is a \linkS4class{QTLExperiment} object,
#' \code{value} is a matrix-like object with the same dimensions as \code{x},
#' and \code{...} are further arguments passed to \code{\link{feature_id}}
#' (for the getter) or \code{\link{feature_id<-}} (for the setter).
#'
#' \describe{
#' \item{\code{feature_id(x, ...)}, \code{feature_id(x, ...) <- value}:}{
#' Get or set the feature (e.g. gene, metabolite) names.
#' }
#' \item{\code{variant_id(x, ...)}, \code{variant_id(x, ...) <- value}:}{
#' Get or set the variant (i.e. SNP) names.
#' }
#' }
#'
#' @details
#' QTL are associations between a genetic variants and a quantitative feature.
#' The \code{\link{feature_id}} and \code{\link{variant_id}} methods can be used
#' to get or set feature IDs and variant IDs, respectively, across a
#' \linkS4class{QTLExperiment} object. The values are stored in the
#' \code{\link{rowData}} and in the \code{\link{int_rowData}} compartments so
#' they can be easily accessed but not accidentally removed or overwritten.
#'
#' @return
#' For \code{feature_id}, a vector is returned containing the name of the
#' feature tested in each association.
#' For \code{feature_id<-}, a modified \code{object} is returned with the
#' updated feature_ids in \code{\link{rowData}}, \code{\link{int_rowData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
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
#' qtle <- mockQTLE()
#' feature_id(qtle) <- sample(LETTERS, nrow(qtle), replace=TRUE)
#' feature_id(qtle)
#' variant_id(qtle) <- sample(paste0("rsid", 1:100), nrow(qtle), replace=TRUE)
#' variant_id(qtle)
#'
#' @name qtle-row_ids
#' @rdname row_ids
#' @docType methods
#' @aliases
#' feature_id
#' feature_id<-
#' feature_id,QTLExperiment-method
#' feature_id<-,QTLExperiment-method
#' variant_id
#' variant_id<-
#' variant_id,QTLExperiment-method
#' variant_id<-,QTLExperiment-method
NULL

.feat_field <- "feature_id"
.var_field <- "variant_id"


#' @importFrom SummarizedExperiment rowData
#'
GET_ROWDATA_FUN <- function(values, ...) {
    (values) # To ensure evaluation
    function(object, ...) {
        output <- rowData(object)[[values]]
        output
    }
}

#' @importFrom SummarizedExperiment rowData rowData<-
#'
SET_ROWDATA_FUN <- function(values, ...) {
    (values)
    function(object, ..., value) {
        object <- updateObject(object)
        int_rowData(object)[[paste0(".", values)]] <- value
        object <- recover_qtle_ids(object)
        object
    }
}


#' @export
setMethod("feature_id", "QTLExperiment", GET_ROWDATA_FUN("feature_id"))

#' @export
setReplaceMethod("feature_id", c("QTLExperiment", "ANY"), SET_ROWDATA_FUN("feature_id"))

#' @export
setMethod("variant_id", "QTLExperiment", GET_ROWDATA_FUN("variant_id"))

#' @export
setReplaceMethod("variant_id", c("QTLExperiment", "ANY"), SET_ROWDATA_FUN("variant_id"))


