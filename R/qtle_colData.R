#' @title
#' Modify and view the colData of a QTLExperiment
#'
#' @description
#' Methods for changing the \code{\link{colData}} of a QTLExperiment. 
#'
#' @details
#' The \code{state_id} column in the \code{colData} is protected, and operations 
#' ensure that this column is not removed from the colData. 
#'
#' @param x is a \linkS4class{QTLExperiment} object
#' @param value is a matrix-like object with number of rows equal 
#' to the number of columns in \code{x}.

#' @return
#' For \code{colData}, a DFrame is returned. 
#' For \code{colData<-}, a modified \linkS4class{QTLExperiment} object is returned with the
#' updated \code{\link{colData}}.
#'
#' @author
#' Christina B Azodi, Amelia Dunstone
#'
#' @examples
#' qtle <- mockQTLE()
#' colData(qtle)
#' dim(colData(qtle))
#' 
#' qtle$batch <- "batch1"
#' colData(qtle)
#' 
#' # The state_id column is protected
#' colData(qtle) <- NULL
#' colData(qtle)
#' 
#' @name qtle-colData
#' @rdname colData
#' @docType methods
#' @aliases
#' colData
#' colData<-
#' colData,QTLExperiment-method
#' colData<-,QTLExperiment,ANY-method

NULL


# Code based on SpatialExperiment-colData

#' @rdname colData
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod(
        "colData", c("QTLExperiment", "DataFrame"), function(x, value) {
            
    # store original 'colData'
    old <- colData(x)
    
    # do the replacement
    se <- as(x, "SummarizedExperiment")
    colData(se) <- value
    new <- colData(se)
    
    if (!is.null(new$state_id)) {
        # Check that new state_id's are valid and give an error if they are not.
        sids_old <- unique(old$state_id)
        sids_new <- unique(new$state_id)
        ns_old <- length(sids_old)
        ns_new <- length(sids_new)
        if (ns_old != ns_new) {
            stop(sprintf(
                "Number of unique 'state_id's is %s, but %s %s provided.\n",
                ns_old, ns_new, ifelse(ns_new > 1, "were", "was")))
        }
    } else {
        # if none provided, retain original state_id field
        value$state_id <- old$state_id
    }
    
    row.names(value) <- value$state_id
    
    BiocGenerics:::replaceSlots(x, colData=value, check=FALSE)
})

#' @rdname colData
#' @importFrom SummarizedExperiment colData colData<-
#' @export
setReplaceMethod("colData", c("QTLExperiment", "NULL"),
    function(x, value) {
        # All columns removed except state_id
        value <- colData(x)["state_id"]
        colData(x) <- value
        return(x)
    }
)

