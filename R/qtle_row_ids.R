#' @title
#' Named rowData getters and setters
#'
#' @description
#' These are methods for getting or setting protected rowData columns (i.e.
#' feature_id and variant_id).
#'
#' @section Available methods:
#' Here \code{object} is a \linkS4class{QTLExperiment} object,
#' \code{value} is a vector-like object with compatible dimensions to \code{object}.
#'
#' \describe{
#' \item{\code{feature_id(object)}:}{
#' Get the feature names.
#' }
#' \item{\code{feature_id(object) <- value}:}{
#' Set the feature names.
#' }
#' \item{\code{variant_id(object)}:}{
#' Get the variant names.
#' }
#' \item{\code{variant_id(object) <- value}:}{
#' Set the variant names.
#' }
#' }
#' 
#' @param object is a \linkS4class{QTLExperiment} object.
#' @param value is a vector with length equal to the number of rows of x. 
#'
#' @details
#' QTL are associations between a genetic variants and a quantitative feature.
#' The \code{\link{feature_id}} and \code{\link{variant_id}} methods can be used
#' to get or set feature IDs and variant IDs, respectively, across a
#' \linkS4class{QTLExperiment} object. The values are stored in the
#' \code{\link{rowData}} compartments and have additional protections 
#' to prevent them being removed or overwritten. 
#' The \code{\link{feature_id}} can store gene or metabolite names, while 
#' \code{\link{variant_id}} could be used to store variant information such
#' as SNP names. 
#'
#' @return
#' For \code{feature_id}, a vector is returned containing the name of the
#' feature tested in each association.
#' For \code{feature_id<-}, a modified \code{object} is returned with the
#' updated feature_ids in \code{\link{rowData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#' For \code{variant_id}, a vector is returned containing the name of the
#' variant tested in each association.
#' For \code{variant_id<-}, a modified \code{object} is returned with the
#' updated variant_ids in \code{\link{rowData}}, and
#' in the row.names of the \linkS4class{QTLExperiment} object.
#'
#' @seealso
#' \linkS4class{QTLExperiment}, for the underlying class definition.
#'
#' @author Christina B Azodi, Amelia Dunstone
#'
#' @examples
#' qtle <- mockQTLE()
#' feature_id(qtle) <- gsub("gene", "Gene", feature_id(qtle))
#' feature_id(qtle)
#' variant_id(qtle) <- paste0(variant_id(qtle), "000")
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
GET_ROWDATA_FUN <- function(field) {
    (field) # To ensure evaluation
    function(object) {
        out <- rowData(object)[[field]]
        out
    }
}

#' @importFrom SummarizedExperiment rowData rowData<-
#'
SET_ROWDATA_FUN <- function(field) {
    (field)
    function(object, value) {
        # object <- updateObject(object)
        rowData(object)[[field]] <- value
        # object <- recover_qtle_ids(object)
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

