#' Reduced dimensions methods
#'
#' Methods to get or set dimensionality reduction results in a
#' \linkS4class{multiStateQTLExperiment} object. Each row of a reduced dimension
#' result corresponds to a column (i.e., state) of the multiStateQTLExperiment
#' object.
#'
#' @section Getters:
#' In the following examples, \code{x} is a
#' \linkS4class{multiStateQTLExperiment} object.
#' \describe{
#' \item{\code{reducedDim(x, type, withDimnames=TRUE)}:}{
#' Retrieves a matrix (or matrix-like object) containing reduced dimension
#' coordinates for states (rows) and dimensions (columns).
#' \code{type} is either a string specifying the name of the dimensionality
#' reduction result in \code{x} to retrieve, or a numeric scalar specifying the
#' index of the desired result, defaulting to the first entry if missing.
#'
#' If \code{withDimnames=TRUE}, row names of the output matrix are replaced
#' with the column names of \code{x}.
#' }
#'
#' \item{\code{reducedDimNames(x)}:}{
#' Returns a character vector containing the names of all dimensionality
#' reduction results in \code{x}. This will be of the same length as the number
#' of results, though the names may not be unique.
#' }
#'
#' \item{\code{reducedDims(x, withDimnames=TRUE)}:}{
#' Returns a named \linkS4class{List} of matrices containing one or more
#' dimensionality reduction results as a matrix (or matrix-like object) with the
#'  same number of rows as \code{ncol(x)}.
#'
#' If \code{withDimnames=TRUE}, row names of each matrix are replaced with the
#' column names of \code{x}.
#' }
#' }
#'
#' @section Single-result setter:
#' \code{reducedDim(x, type, withDimnames=TRUE) <- value} will add or replace a
#' dimensionality reduction result in a \linkS4class{multiStateQTLExperiment}
#' object \code{x}. The value of \code{type} determines how the result is added
#' or replaced:
#' \itemize{
#' \item If \code{type} is missing, \code{value} is assigned to the first
#' result. If the result already exists, its name is preserved; otherwise it is
#' given a default name \code{"unnamed1"}.
#' \item If \code{type} is a numeric scalar, it must be within the range of
#' existing results, and \code{value} will be assigned to the result at that
#' index.
#' \item If \code{type} is a string and a result exists with this name,
#' \code{value} is assigned to to that result. Otherwise a new result with this
#'  name is append to the existing list of results.
#' }
#'
#' \code{value} is expected to be a matrix or matrix-like object with number of
#' rows equal to \code{ncol(x)}. Alternatively, if \code{value} is \code{NULL},
#' the result corresponding to \code{type} is removed from the object.
#'
#' If \code{withDimnames=TRUE}, any non-\code{NULL} \code{rownames(value)} is
#' checked against \code{colnames(x)} and a warning is emitted if they are not
#' the same. Otherwise, any differences in the row names are ignored. This
#' raises a warning rather than an error and allows \code{NULL} rownames to
#' pass through without complaints.
#'
#' @section Other setters:
#' Here \code{x} is a \linkS4class{multiStateQTLExperiment} object.
#' \describe{
#' \item{\code{reducedDims(x, withDimnames=TRUE) <- value}:}{
#' Replaces all dimensionality reduction results in \code{x} with those in
#' \code{value}. The latter should be a list-like object containing matrices or
#' matrix-like objects with number of rows equal to \code{ncol(x)}.
#'
#' If \code{value} is named, those names will be used to name the dimensionality
#' reduction results in \code{x}. Otherwise, unnamed results are assigned
#' default names prefixed with \code{"unnamed"}.
#'
#' If \code{value} is \code{NULL}, all dimensionality reduction results in
#' \code{x} are removed.
#'
#' If \code{value} is a \linkS4class{Annotated} object, any
#' \code{\link{metadata}} will be retained in \code{reducedDims(x)}.
#' If \code{value} is a \linkS4class{Vector} object, any \code{\link{mcols}}
#' will also be retained.
#'
#' If \code{withDimnames=TRUE}, any non-\code{NULL} row names in each entry of
#' \code{value} is checked against \code{colnames(x)} and a warning is emitted
#' if they are not the same. Otherwise, differences in row names are ignored.
#' }
#' \item{\code{reducedDimNames(x) <- value}:}{
#' Replaces all names for dimensionality reduction results in \code{x} with a
#' character vector \code{value}. This should be of length equal to the number
#' of results currently in \code{x}.
#' }
#' }
#'
#' @section Storing dimensionality reduction metadata:
#' Dimension reduction methods frequently generate metadata that we want to
#' store, for example the percentage of variance explained and the rotation
#' matrix from PCA. To store this information alongside the coordinates, we
#' recommended that this metadata is stored as attributes of the coordinate
#' matrix. This is simple to do, easy to extract, and avoids problems with
#' synchronization (when the coordinates are separated from the metadata).
#' The biggest problem with this approach is that attributes are not retained
#' when the matrix is subsetted or combined. To persist these attributes, we
#' suggest wrapping the coordinates and metadata in a
#' \link[SingleCellExperiment]{reduced.dim.matrix}.
#'
#' @author Christina B Azodi
#'
#' @examples
#' msqe <- mockMSQE()
#' reducedDim(msqe, "PCA") <- prcomp(t(betas(msqe)), rank=50)$x
#' reducedDims(msqe)
#' reducedDims(msqe)$PCA
#' reducedDims(msqe) <- SimpleList()
#' reducedDims(msqe)
#'
#' @name reducedDims
#' @docType methods
#' @aliases
#' reducedDim reducedDims reducedDimNames
#' reducedDim,multiStateQTLExperiment,missing-method
#' reducedDim,multiStateQTLExperiment,numeric-method
#' reducedDim,multiStateQTLExperiment,character-method
#' reducedDims,multiStateQTLExperiment-method
#' reducedDimNames,multiStateQTLExperiment-method
#' reducedDim<- reducedDims<- reducedDimNames<-
#' reducedDim<-,multiStateQTLExperiment,missing-method
#' reducedDim<-,multiStateQTLExperiment,numeric-method
#' reducedDim<-,multiStateQTLExperiment,character-method
#' reducedDims<-,multiStateQTLExperiment-method
#' reducedDimNames<-,multiStateQTLExperiment,character-method
NULL

.red_key <- "reducedDims"

#' @export
setMethod("reducedDims", "multiStateQTLExperiment",
          function(x, withDimnames=TRUE) {
  value <- .get_internal_all(x,
                             getfun=int_colData,
                             key=.red_key)

  if (withDimnames) {
    for (i in seq_along(value)) {
      rownames(value[[i]]) <- colnames(x)
    }
  }
  value
})

#' @export
setReplaceMethod("reducedDims", "multiStateQTLExperiment",
                 function(x, withDimnames=TRUE, ..., value) {
  if (withDimnames) {
    for (v in seq_along(value)) {
      .check_reddim_names(x, value[[v]], withDimnames=TRUE,
                          vname=sprintf("value[[%s]]", v), fun='reducedDims')
    }
  }

  .set_internal_all(x, value,
                    getfun=int_colData,
                    setfun=`int_colData<-`,
                    key=.red_key,
                    convertfun=NULL,
                    xdimfun=ncol,
                    vdimfun=nrow,
                    funstr="reducedDims",
                    xdimstr="ncol",
                    vdimstr="rows")
})

#' @export
setMethod("reducedDimNames", "multiStateQTLExperiment", function(x) {
  .get_internal_names(x,
                      getfun=int_colData,
                      key=.red_key)
})

#' @export
setReplaceMethod("reducedDimNames", c("multiStateQTLExperiment", "character"),
                 function(x, value) {
  .set_internal_names(x, value,
                      getfun=int_colData,
                      setfun=`int_colData<-`,
                      key=.red_key)
})

#' @export
setMethod("reducedDim", c("multiStateQTLExperiment", "missing"),
          function(x, type, withDimnames=TRUE) {
  .get_internal_missing(x,
                        basefun=reducedDim,
                        namefun=reducedDimNames,
                        funstr="reducedDim",
                        withDimnames=withDimnames)
})

#' @export
setMethod("reducedDim", c("multiStateQTLExperiment", "numeric"),
          function(x, type, withDimnames=TRUE) {
  out <- .get_internal_integer(x, type,
                               getfun=int_colData,
                               key=.red_key,
                               funstr="reducedDim",
                               substr="type")

  if (withDimnames) {
    rownames(out) <- colnames(x)
  }

  out
})

#' @export
setMethod("reducedDim", c("multiStateQTLExperiment", "character"),
          function(x, type, withDimnames=TRUE) {
  out <- .get_internal_character(x, type,
                                 getfun=int_colData,
                                 key=.red_key,
                                 funstr="reducedDim",
                                 substr="type",
                                 namestr="reducedDimNames")

  if (withDimnames) {
    rownames(out) <- colnames(x)
  }

  out
})

.check_reddim_names <- function(reference, incoming, withDimnames,
                                fun='reducedDim', vname='value') {
  if (!is.null(incoming)) {
    rni <- rownames(incoming)
    cnr <- colnames(reference)
    if (withDimnames && !is.null(rni)) {
      if (!identical(cnr, rni)) {
        stop("non-NULL 'rownames(", vname, ")' should be the same as ",
             "'colnames(x)' for '", fun, "<-'.")
      }
    }
  }
  incoming
}

#' @export
setReplaceMethod("reducedDim", c("multiStateQTLExperiment", "missing"),
                 function(x, type, withDimnames=TRUE, ..., value) {
  .set_internal_missing(x, value,
                        withDimnames=withDimnames,
                        basefun=`reducedDim<-`,
                        namefun=reducedDimNames
  )
})

#' @export
setReplaceMethod("reducedDim", c("multiStateQTLExperiment", "numeric"),
                 function(x, type, withDimnames=TRUE, ..., value) {
  .check_reddim_names(x, value, withDimnames)

  .set_internal_numeric(x, type, value,
                        getfun=int_colData,
                        setfun=`int_colData<-`,
                        key=.red_key,
                        convertfun=NULL,
                        xdimfun=ncol,
                        vdimfun=nrow,
                        funstr="reducedDim",
                        xdimstr="ncol",
                        vdimstr="rows",
                        substr="type")
})

#' @export
setReplaceMethod("reducedDim", c("multiStateQTLExperiment", "character"),
                 function(x, type, withDimnames=TRUE, ..., value) {
  .check_reddim_names(x, value, withDimnames)

  .set_internal_character(x, type, value,
                          getfun=int_colData,
                          setfun=`int_colData<-`,
                          key=.red_key,
                          convertfun=NULL,
                          xdimfun=ncol,
                          vdimfun=nrow,
                          funstr="reducedDim",
                          xdimstr="ncol",
                          vdimstr="rows",
                          substr="type")
})


