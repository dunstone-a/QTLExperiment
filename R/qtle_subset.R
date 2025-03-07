#' @title Subsetting and replacing data in QTLExperiment objects
#'
#' @description
#' Includes methods to subset a \linkS4class{QTLExperiment} object by
#' row and/or column and methods to replace all data for the specified rows
#' and/or columns with another value. These methods ensure that all data fields
#' remain synchronized when states or associations are removed.
#'
#' @section Subsetting:
#' In the following, \code{x} is a \linkS4class{QTLExperiment} object.
#'
#' \describe{
#' \item{\code{x[i, j, ..., drop=TRUE]}:}{Returns a QTLExperiment
#'  containing the specified rows \code{i} and columns \code{j}, where \code{i}
#'  and \code{j} can be a logical, integer or character vector of subscripts,
#'  indicating the rows and columns, respectively, to retain. If either \code{i}
#'  or \code{j} is missing, than subsetting is only performed in the specified
#'  dimension. Arguments in \code{...} and \code{drop} are passed to
#'  \code{\link{[,SummarizedExperiment-method}}.}
#'}
#'
#' @section Replacing:
#' In the following, \code{x} is a \linkS4class{QTLExperiment} object.
#'
#' \describe{
#' \item{\code{x[i, j, ...] <- value}:}{Replaces all data for rows \code{i} and
#' columns \code{j} with the corresponding fields in a QTLExperiment
#' \code{value}, where \code{i} and \code{j} can be a logical, integer, or
#' character vector of subscripts, indicating the rows and columns,
#' respectively, to retain. If either \code{i} or \code{j} is missing, than
#' subsetting is only performed in the specified dimension. If both are missing,
#' \code{x} is replaced entirely with \code{value}. Arguments in \code{...} are
#' passed to the corresponding \linkS4class{SummarizedExperiment} method.}
#' }
#'
#' @returns A \linkS4class{QTLExperiment} object.
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLE()
#'
#' # Subsetting:
#' qtle[1:10,]
#' qtle[,1:5]
#'
#' # Can also use subset()
#' qtle$WHEE <- sample(c("A", "B", "C"), ncol(qtle), replace=TRUE)
#' subset(qtle, , WHEE=="A")
#'
#' # Can also use split()
#' split(qtle, sample(c("A", "B", "C"), nrow(qtle), replace=TRUE))
#'
#' @docType methods
#' @aliases
#' [,QTLExperiment,ANY-method
#' [,QTLExperiment,ANY,ANY-method
#' [,QTLExperiment,ANY,ANY,ANY-method
#' [<-,QTLExperiment,ANY,ANY,QTLExperiment-method
#'
#' @name QTLe-subset
#' @rdname subset
NULL


#' @export
setMethod("[", c("QTLExperiment", "ANY", "ANY"), function(x, i, j, ...,
                                                          drop=TRUE) {

    out_row <- rowData(x)
    out_col <- colData(x)
    
    if (!missing(i)) {
        ii <- .convert_subset_index(i, rownames(x))
        out_row <- rowData(x)[ii,,drop=FALSE]
    }

    if (!missing(j)) {
        jj <- .convert_subset_index(j, colnames(x))
        out_col <- colData(x)[jj,,drop=FALSE]
    }
    
    # out is invalid until all slots have been updated
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    
    out <- callNextMethod()
    BiocGenerics:::replaceSlots(out, colData = out_col, elementMetadata = out_row, check=FALSE)
    
    validObject(out)
    
    out
})


#' @export
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importFrom SummarizedExperiment rowData colData
setMethod("[<-", c("QTLExperiment", "ANY", "ANY",
                   "QTLExperiment"), function(x, i, j, ..., value) {
                       
    out_row <- rowData(x)
    out_col <- colData(x)
                       
    if (missing(i) && missing(j)) {
        return(value)
    }
    
    if (!missing(i)) {
        left <- rowData(x)
        right <- rowData(value)
        ii <- .convert_subset_index(i, rownames(x))
    
        tryCatch({ left[ii,] <- right
        }, error=function(err) {
            stop(
                "failed to replace 'rowData' in '<", class(x),
                ">[i,] <- value'\n", conditionMessage(err))
        })
        out_row <- left
    }
    
    if (!missing(j)) {
        left <- colData(x)
        right <-colData(value)
        jj <- .convert_subset_index(j, colnames(x))
        
        tryCatch({ left[jj,] <- right
        }, error=function(err) {
            stop("failed to replace 'colData' in '<", class(x),
                ">[,j] <- value'\n", conditionMessage(err)) 
        })
        out_col <- left
    }
    
    out_metadata <- int_metadata(value)
    
    # out is invalid until all slots have been updated
    old <- S4Vectors:::disableValidity()
    if (!isTRUE(old)) {
        S4Vectors:::disableValidity(TRUE)
        on.exit(S4Vectors:::disableValidity(old))
    }
    
    out <- callNextMethod()
    
    BiocGenerics:::replaceSlots(out, colData=out_col, elementMetadata=out_row,
        int_metadata=out_metadata, check=FALSE)
    
    validObject(out)
    
    out
})

#' @importFrom S4Vectors parallel_slot_names
setMethod("parallel_slot_names", "QTLExperiment", function(x) {
    c("rowRanges", "assays", "NAMES", "elementMetadata")
})

