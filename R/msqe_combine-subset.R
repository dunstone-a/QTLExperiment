#' @title Combining or subsetting multiStateQTLExperiment objects
#'
#' @description
#' An overview of methods to combine multiple \linkS4class{multiStateQTLExperiment}
#' objects by row or column, or to subset a multiStateQTLExperiment by row or
#' column. These methods ensure that all data fields remain synchronized when
#' states or associations are added or removed.
#'
#'
#' @section Combining:
#' In the following code snippets, \code{...} contains one or more
#' \linkS4class{multiStateQTLExperiment} objects.
#' \describe{
#' \item{\code{rbind(..., deparse.level=1)}:}{Returns a multiStateQTLExperiment 
#' where all objects are combined row-wise.
#'
#' Note that all objects in \code{...} must have the exact same values for
#' \code{\link{reducedDims}}.
#' 
#' Metadata is combined as in \code{?"\link{rbind,SummarizedExperiment-method}"}.
#' The \code{deparse.level} specifies how row.names are generated as described 
#' in \code{?\link[base]{rbind}}.
#' }
#'
#' \item{\code{cbind(..., deparse.level=1)}:}{Returns a multiStateQTLExperiment
#' where all objects are combined column-wise.
#'
#' Note that all objects in \code{...} must have the exact same values for
#' \code{\link{reducedDims}} (though they can be unordered). Dimensionality 
#' reduction results with the same name across objects will be combined row-wise
#' to create the corresponding entry in the output object.
#'
#' Metadata is combined as in \code{?"\link{cbind,SummarizedExperiment-method}"}.
#' The \code{deparse.level} specifies how colnames are generated as described 
#' in \code{?\link[base]{cbind}}.
#' }
#' }
#'
#'
#' @section Subsetting:
#' In the following, \code{x} is a \linkS4class{multiStateQTLExperiment} object.
#'
#' \describe{
#' \item{\code{x[i, j, ..., drop=TRUE]}:}{Returns a multiStateQTLExperiment
#'  containing the specified rows \code{i} and columns \code{j}.
#'
#' \code{i} and \code{j} can be a logical, integer or character vector of
#' subscripts, indicating the rows and columns respectively to retain. If either
#' is missing, subsetting is only performed in the specified dimension.
#'
#' Arguments in \code{...} and \code{drop} are passed to
#' \code{\link{[,SummarizedExperiment-method}}.}
#'
#' \item{\code{x[i, j, ...] <- value}:}{Replaces all data for rows \code{i} and
#' columns {j} with the corresponding fields in a multiStateQTLExperiment
#' \code{value}.
#'
#' \code{i} and \code{j} can be a logical, integer or character vector of
#' subscripts, indicating the rows and columns respectively to retain. If either
#' is missing, subsetting is only performed in the specified dimension.
#' If both are missing, \code{x} is replaced entirely with \code{value}.
#'
#' Arguments in \code{...} are passed to the corresponding
#' \linkS4class{SummarizedExperiment} method.}
#' }
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' msqe <- mockMSQE()
#'
#' # Combining:
#' rbind(msqe, msqe)
#' cbind(msqe, msqe)
#'
#' # Subsetting:
#' msqe[1:10,]
#' msqe[,1:5]
#'
#' msqe2 <- msqe
#' msqe2[1:10,] <- msqe[11:20,]
#'
#' # Can also use subset()
#' sce$WHEE <- sample(LETTERS, ncol(sce), replace=TRUE)
#' subset(sce, , WHEE=="A")
#'
#' # Can also use split()
#' split(sce, sample(LETTERS, nrow(sce), replace=TRUE))
#'
#' @docType methods
#' @aliases
#' cbind,multiStateQTLExperiment-method
#' rbind,multiStateQTLExperiment-method
#' [,multiStateQTLExperiment,ANY-method
#' [,multiStateQTLExperiment,ANY,ANY-method
#' [,multiStateQTLExperiment,ANY,ANY,ANY-method
#' [<-,multiStateQTLExperiment,ANY,ANY,multiStateQTLExperiment-method
#'
#' @name MSQE-combine
#' @rdname combine
NULL

#' @export
#' @importFrom BiocGenerics rbind cbind 
#' 
setMethod("cbind", "multiStateQTLExperiment", function(..., deparse.level=1) {
  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }
  out <- callNextMethod()

  args <- list(...)
  args <- lapply(args, updateObject)
  int_meta <- do.call(c, unname(lapply(args, int_metadata)))

  tryCatch({
    int_colD <- do.call(rbind, lapply(args, int_colData))
  }, error=function(err) {
    stop(
      "failed to combine 'int_colData' in 'cbind(<", class(args[[1]]), ">)':\n",
      conditionMessage(err))
  })

  # Creating a shell to avoid having to pull out .cbind.DataFrame
  # to fuse metadata along the dimension not being combined.
  row_shells <- lapply(args, .create_shell_rowdata)
  tryCatch({
    combined <- do.call(SummarizedExperiment::cbind, row_shells) ## In SCE, SummarizedExperiment::cbind did not need to be specified
  }, error=function(err) {
    stop( "failed to combine 'int_elementMetadata' in 'cbind(<", 
          class(args[[1]]), ">)':\n  ", conditionMessage(err))
  })
  int_eleMetaD <- rowData(combined)

  BiocGenerics:::replaceSlots(out, int_colData=int_colD,
               int_elementMetadata=int_eleMetaD,
               int_metadata=int_meta, check=FALSE)
})

#' @export
#' @importFrom BiocGenerics rbind cbind
setMethod("rbind", "multiStateQTLExperiment", function(..., deparse.level=1) {
  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }
  out <- callNextMethod()

  args <- list(...)
  args <- lapply(args, updateObject)
  int_meta <- do.call(c, unname(lapply(args, int_metadata)))

  tryCatch({
    int_eleMetaD <- do.call(rbind, lapply(args, int_elementMetadata))
  }, error=function(err) {
    stop("failed to combine 'int_elementMetadata' in 'rbind(<",
         class(args[[1]]), ">)':\n  ", conditionMessage(err))
  })

  # Creating a shell to avoid having to pull out .cbind.DataFrame
  # to fuse metadata along the dimension not being combined.
  col_shells <- lapply(args, .create_shell_coldata)
  tryCatch({
    combined <- do.call(SummarizedExperiment::rbind, col_shells)
  }, error=function(err) {
    stop("failed to combine 'int_colData' in 'rbind(<", class(args[[1]]),
         ">)'\n", conditionMessage(err))
  })
  int_colD <- colData(combined)

  BiocGenerics:::replaceSlots(out, int_colData=int_colD,
               int_elementMetadata=int_eleMetaD,
               int_metadata=int_meta, check=FALSE)
})

#' @importFrom SummarizedExperiment SummarizedExperiment
.create_shell_coldata <- function(x) {
  SummarizedExperiment(colData=int_colData(x))
}

#' @importFrom SummarizedExperiment SummarizedExperiment
.create_shell_rowdata <- function(x) {
  SummarizedExperiment(rowData=int_elementMetadata(x))
}


#' @export
setMethod("[", c("multiStateQTLExperiment", "ANY", "ANY"), function(x, i, j, ..., 
                                                                    drop=TRUE) {
  x <- updateObject(x)
  if (!missing(i)) {
    ii <- .convert_subset_index(i, rownames(x))
    int_elementMetadata(x) <- int_elementMetadata(x)[ii,,drop=FALSE]
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
  message(i)
  message("j")
  message(j)
  if (!missing(i)) {
    left <- int_elementMetadata(x)
    right <- int_elementMetadata(value)
    ii <- .convert_subset_index(i, rownames(x))
    
    tryCatch({ left[ii,] <- right
    }, error=function(err) {
      stop(
        "failed to replace 'int_elementMetadata' in '<", class(x),
        ">[i,] <- value'\n", conditionMessage(err))
    })
    int_elementMetadata(x) <- left
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