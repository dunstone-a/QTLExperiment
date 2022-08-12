#' @title Subsetting and replacing data in multiStateQTLExperiment objects
#'
#' @description
#' Includes methods to subset a \linkS4class{multiStateQTLExperiment} object by
#' row and/or column and methods to replace all data for the specified rows
#' and/or columns with another value. These methods ensure that all data fields
#' remain synchronized when states or associations are removed.
#'
#' @section Subsetting:
#' In the following, \code{x} is a \linkS4class{multiStateQTLExperiment} object.
#'
#' \describe{
#' \item{\code{x[i, j, ..., drop=TRUE]}:}{Returns a multiStateQTLExperiment
#'  containing the specified rows \code{i} and columns \code{j}, where \code{i}
#'  and \code{j} can be a logical, integer or character vector of subscripts,
#'  indicating the rows and columns, respectively, to retain. If either \code{i}
#'  or \code{j} is missing, than subsetting is only performed in the specified
#'  dimension. Arguments in \code{...} and \code{drop} are passed to
#'  \code{\link{[,SummarizedExperiment-method}}.}
#'}
#'
#' @section Replacing:
#' In the following, \code{x} is a \linkS4class{multiStateQTLExperiment} object.
#'
#' \describe{
#' \item{\code{x[i, j, ...] <- value}:}{Replaces all data for rows \code{i} and
#' columns {j} with the corresponding fields in a multiStateQTLExperiment
#' \code{value}, where \code{i} and \code{j} can be a logical, integer, or
#' character vector of subscripts, indicating the rows and columns,
#' respectively, to retain. If either \code{i} or \code{j} is missing, than
#' subsetting is only performed in the specified dimension. If both are missing,
#' \code{x} is replaced entirely with \code{value}. Arguments in \code{...} are
#' passed to the corresponding \linkS4class{SummarizedExperiment} method.}
#' }
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' msqe <- mockMSQE()
#'
#' # Subsetting:
#' msqe[1:10,]
#' msqe[,1:5]
#'
#' msqe2 <- msqe
#' msqe2[1:10,] <- msqe[11:20,]
#'
#' # Can also use subset()
#' msqe$WHEE <- sample(c("A", "B", "C"), ncol(msqe), replace=TRUE)
#' subset(msqe, , WHEE=="A")
#'
#' # Can also use split()
#' split(msqe, sample(c("A", "B", "C"), nrow(msqe), replace=TRUE))
#'
#' @docType methods
#' @aliases
#' [,multiStateQTLExperiment,ANY-method
#' [,multiStateQTLExperiment,ANY,ANY-method
#' [,multiStateQTLExperiment,ANY,ANY,ANY-method
#' [<-,multiStateQTLExperiment,ANY,ANY,multiStateQTLExperiment-method
#'
#' @name MSQE-subset
#' @rdname subset
NULL



#' @export
setMethod("[", c("multiStateQTLExperiment", "ANY", "ANY"), function(x, i, j, ...,
                                                                    drop=TRUE) {
  x <- updateObject(x)
  if (!missing(i)) {
    ii <- .convert_subset_index(i, rownames(x))
    int_rowData(x) <- int_rowData(x)[ii,,drop=FALSE]
  }

  if (!missing(j)) {
    jj <- .convert_subset_index(j, colnames(x))
    int_colData(x) <- int_colData(x)[jj,,drop=FALSE]
  }

  callNextMethod()
})


#' @export
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importFrom SummarizedExperiment rowData colData
setMethod("[<-", c("multiStateQTLExperiment", "ANY", "ANY", "multiStateQTLExperiment"), function(x, i, j, ..., value) {
  x <- updateObject(x)
  value <- updateObject(value)
  if (missing(i) && missing(j)) {
    return(value)
  }

  if (!missing(i)) {
    left <- int_rowData(x)
    right <- int_rowData(value)
    ii <- .convert_subset_index(i, rownames(x))

    tryCatch({ left[ii,] <- right
    }, error=function(err) {
      stop(
        "failed to replace 'int_rowData' in '<", class(x),
        ">[i,] <- value'\n", conditionMessage(err))
    })
    int_rowData(x) <- left
  }

  if (!missing(j)) {
    left <- int_colData(x)
    right <- int_colData(value)
    jj <- .convert_subset_index(j, colnames(x))

    tryCatch({ left[jj,] <- right
    }, error=function(err) {
      stop("failed to replace 'int_colData' in '<", class(x),
           ">[,j] <- value'\n", conditionMessage(err))
    })
    int_colData(x) <- left
  }

  int_metadata(x) <- int_metadata(value)
  callNextMethod()
})
