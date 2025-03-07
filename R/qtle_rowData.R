#' @title
#' rowData method for QTLExperiment
#'
#' @description
#' Methods for changing the \code{\link{rowData}} of a QTLExperiment. 
#'
#' @details
#' The \code{feature_id} and \code{variant_id} columns in the \code{rowData} are protected, and operations 
#' ensure that these columns are preserved in the rowData. 
#'
#'
#' @param x is a \linkS4class{QTLExperiment} object
#' @param value is a matrix-like object with number of rows equal 
#' to the number of rows in \code{x}.
#' @param use.names is a logical specifying whether or not to propogate 
#' the rownames of \code{x} to the returned DFrame object. 

#' @return
#' For \code{rowData}, a DFrame is returned. 
#' For \code{rowData<-}, a modified \linkS4class{QTLExperiment} object is returned with the
#' updated \code{\link{rowData}}.
#'
#' @author
#' Christina B Azodi, Amelia Dunstone
#'
#' @examples
#' qtle <- mockQTLE()
#' rowData(qtle)
#' dim(rowData(qtle))
#' 
#' rowData(qtle)$chr <- ifelse(feature_id(qtle) %in% c("geneA", "geneB"), "chr1", "chr2")
#' rowData(qtle)
#' 
#' # The state_id column is protected
#' rowData(qtle) <- NULL
#' rowData(qtle)
#' 
#' @name qtle-rowData
#' @rdname rowData
#' @docType methods
#' @aliases
#' rowData
#' rowData<-
#' rowData,QTLExperiment-method
#' rowData<-,QTLExperiment-method

#' @export
setMethod("rowData", "QTLExperiment", function(x, use.names = TRUE) {
    out <- x@elementMetadata
    # row.names(out) <- paste(out[[.feat_field]], out[[.var_field]], sep="|")
    out
})

#' Code based on SpatialExperiment-colData
#' 
#' @rdname rowData
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment rowData rowData<-
#' @export
setReplaceMethod(
        "rowData", c("QTLExperiment", "DataFrame"), function(x, value) {
        
    # store original 'rowData'
    old <- rowData(x)
    
    # do the replacement
    se <- as(x, "SummarizedExperiment")
    rowData(se) <- value
    new <- rowData(se)
    
    fields <- c(.feat_field, .var_field)
    
    for (field in fields) {
        if (!is.null(new[[field]])) {
            # Check that new fields are valid and give an error if they are not.
            ids_old <- unique(old[[field]])
            ids_new <- unique(new[[field]])
            n_old <- length(ids_old)
            n_new <- length(ids_new)
            
            if (n_old != n_new) {
                stop(sprintf(
                    "Number of unique %s's is %s, but %s %s provided.\n",
                    field, n_old, n_new, ifelse(n_new > 1, "were", "was")))
            } 
            if (field == .feat_field) {
                # Make sure new feature names map uniquely to the old ones
                tab <- table(old[[.feat_field]], new[[.feat_field]])
                if (sum(tab != 0) != n_old) {
                    stop(sprintf(
                        "New %s's must map uniquely", .feat_field))
                }
            }
        } else {
            # if none provided, retain original field
            value[[field]] <- old[[field]]
        }
    }
    
    row.names(value) <- paste(value[[.feat_field]], value[[.var_field]], sep="|")
    row.names(x) <- row.names(value)
    
    BiocGenerics:::replaceSlots(x, elementMetadata=value, check=FALSE)
})

#' @rdname rowData
#' @importFrom SummarizedExperiment rowData rowData<-
#' @export
setReplaceMethod("rowData", c("QTLExperiment", "NULL"),
        function(x, value) {
            
    # All columns removed except state_id
    value <- rowData(x)[c(.feat_field, .var_field)]
    rowData(x) <- value
    return(x)
    }
)
