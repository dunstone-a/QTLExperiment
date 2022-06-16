#' @title
#' Internal multiStateQTLExperiment functions
#'
#' @description
#' Methods to get or set internal fields from the multiStateQTLExperiment class.
#' These functions are intended for package developers who want to add protected
#' fields to a multiStateQTLExperiment. They should \emph{not} be used by
#' ordinary users of the \pkg{multiStateQTLExperiment} package.
#'
#' @section Getters:
#' Here \code{x} is a \linkS4class{multiStateQTLExperiment}.
#' \describe{
#' \item{\code{int_elementMetadata(x)}:}{Returns a \linkS4class{DataFrame} of
#' internal row metadata, with number of rows equal to \code{nrow(x)} (analogous
#' to the user-visible \code{\link{rowData}}).}
#' \item{\code{int_colData(x)}:}{Returns a \linkS4class{DataFrame} of internal
#' column metadata, with number of rows equal to \code{ncol(x)} (analogous to
#' the user-visible \code{\link{colData}}).}
#' \item{\code{int_metadata(x)}:}{Returns a list of internal metadata (analogous
#'  to the user-visible \code{\link{metadata}}).}
#' }
#'
#' The following methods can return visible and internal data in a single
#' DataFrame.
#' \describe{
#' \item{\code{rowData(x, ..., internal=TRUE)}:}{Returns a
#' \linkS4class{DataFrame} of the user-visible row metadata with the internal
#' row metadata added column-wise. A warning is emitted if the user-visible
#' metadata column names overlap with the internal fields. Any arguments in
#' \code{...} are passed to \code{\link{rowData,SummarizedExperiment-method}}.}
#'
#' \item{\code{colData(x, ..., internal=TRUE)}:}{Returns a
#' \linkS4class{DataFrame} of the user-visible column metadata with the internal
#' column metadata added column-wise. A warning is emitted if the user-visible
#' metadata column names overlap with the internal fields. Any arguments in
#' \code{...} are passed to \code{\link{colData,SummarizedExperiment-method}}.}
#' }
#'
#' @section Setters:
#' Here \code{x} is a \linkS4class{multiStateQTLExperiment}.
#' \describe{
#' \item{\code{int_elementMetadata(x) <- value}:}{Replaces the internal row
#' metadata with \code{value}, a \linkS4class{DataFrame} with number of rows
#' equal to \code{nrow(x)} (analogous to the user-visible
#' \code{\link{rowData<-}}).}
#' \item{\code{int_colData(x) <- value}:}{Replaces the internal column metadata
#'  with \code{value}, a \linkS4class{DataFrame} with number of rows equal to
#'  \code{ncol(x)} (analogous to the user-visible \code{\link{colData<-}}).}
#' \item{\code{int_metadata(x) <- value}:}{Replaces the internal metadata with
#' \code{value} (analogous to the user-visible \code{\link{metadata<-}}).}
#' }
#'
#' @section Comments:
#' The internal metadata fields store additional elements that are parallel to
#' the rows or columns of a \linkS4class{multiStateQTLExperiment} class. This
#' avoids the need to specify new slots and adjust the subsetting/combining code
#' for a new data element. For example, \code{\link{reducedDims}} is
#' implemented as fields in the internal column metadata.
#'
#' These elements being internal is important as it ensures that the
#' implementation details are abstracted away. User interaction with these
#' internal fields should be done via the designated getter and setter methods
#' (e.g., \code{\link{reducedDim}}), providing developers with freedom to change
#' the internal representation without breaking user code.
#'
#' @seealso
#' \code{\link{colData}}, \code{\link{rowData}} and \code{\link{metadata}} for
#' the user-visible equivalents.
#'
#' @author Christina B Azodi
#'
#' @name MSQE-internals
#' @rdname internals
#' @docType methods
#' @aliases
#' int_colData
#' int_elementMetadata
#' int_metadata
#' int_colData,multiStateQTLExperiment-method
#' int_elementMetadata,multiStateQTLExperiment-method
#' int_metadata,multiStateQTLExperiment-method
#' int_colData<-
#' int_elementMetadata<-
#' int_metadata<-
#' int_colData<-,multiStateQTLExperiment-method
#' int_elementMetadata<-,multiStateQTLExperiment-method
#' int_metadata<-,multiStateQTLExperiment-method
#' colData,multiStateQTLExperiment-method
#' rowData,multiStateQTLExperiment-method
#' parallel_slot_names,multiStateQTLExperiment-method
#'
#' @examples
#' msqe <- mockMSQE()
#' int_metadata(msqe)$whee <- 1
NULL

#' @export
setMethod("int_elementMetadata", "multiStateQTLExperiment", function(x) x@int_elementMetadata)

#' @export
setReplaceMethod("int_elementMetadata", "multiStateQTLExperiment", function(x, value) {
    x@int_elementMetadata <- value
    return(x)
})

#' @export
#' @importFrom S4Vectors parallel_slot_names
setMethod("parallel_slot_names", "multiStateQTLExperiment", function(x) {
    c("int_elementMetadata", callNextMethod())
})

#' @export
setMethod("int_colData", "multiStateQTLExperiment", function(x) x@int_colData)

#' @export
setReplaceMethod("int_colData", "multiStateQTLExperiment", function(x, value) {
    x@int_colData <- value
    return(x)
})

#' @export
setMethod("int_metadata", "multiStateQTLExperiment", function(x) x@int_metadata)

#' @export
setReplaceMethod("int_metadata", "multiStateQTLExperiment", function(x, value) {
    x@int_metadata <- value
    return(x)
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod("colData", "multiStateQTLExperiment", function(x, ..., internal=FALSE) {
    if(internal) {
        cn <- colnames(x@colData) # need explicit slot reference to avoid recursive colData() calling.
        conflict <- cn %in% colnames(int_colData(x))
        if (any(conflict)) {
            cn <- cn[conflict]
            if (length(cn) > 2) {
                cn <- c(cn[seq_len(2)], "...")
            }
            warning("overlapping names in internal and external colData (", paste(cn, collapse = ", "), ")")
        }
        cbind(callNextMethod(x, ...), int_colData(x))
    } else {
        callNextMethod(x, ...)
  }
})

#' @export
#' @importFrom S4Vectors mcols
#' @importFrom SummarizedExperiment rowData
setMethod("rowData", "multiStateQTLExperiment", function(x, ..., internal=FALSE) {
    if (internal) {
        cn <- colnames(mcols(x))
        conflict <- cn %in% colnames(int_elementMetadata(x))
        if (any(conflict)) {
            cn <- cn[conflict]
            if (length(cn) > 2) {
                cn <- c(cn[seq_len(2)], "...")
            }
            warning("overlapping names in internal and external rowData (", paste(cn, collapse = ", "), ")")
        }
        cbind(callNextMethod(x, ...), int_elementMetadata(x))
    } else {
        callNextMethod(x, ...)
    }
})

##################################################
### Defining getters/setters for the internals ###
##################################################

#' @importClassesFrom S4Vectors SimpleList
.get_internal_all <- function(x, getfun, key, convertfun) {
    x <- updateObject(x)
    as(getfun(x)[[key]], "SimpleList")
}

#' @importFrom methods as
#' @importFrom S4Vectors DataFrame I mcols mcols<- metadata metadata<-
.set_internal_all <- function(x, value, getfun, setfun, key, convertfun,
                              xdimfun, vdimfun, funstr, xdimstr, vdimstr) {
    x <- updateObject(x)

    if (length(value) == 0L) {
        collected <- getfun(x)[, 0]
    } else {
        original <- value

        if (!is.null(convertfun)) {
            value <- lapply(value, convertfun)
        }

        N <- vapply(value, vdimfun, 0L)
        if (!all(N == xdimfun(x))) {
            stop("invalid 'value' in '", funstr, "(<", class(x), ">) <- value'\n",
                "each element of 'value' should have number of ", vdimstr,
                " equal to '", xdimstr, "(x)'")
        }

        names(value) <- .clean_internal_names(names(value), N=length(value), msg="names(value)")
        collected <- do.call(DataFrame, c(lapply(value, I), list(row.names=NULL, check.names=FALSE)))

        if (is(original, "Annotated")) {
            metadata(collected) <- metadata(original)
        }
        if (is(original, "Vector")) {
            mcols(collected) <- mcols(original)
        }
    }

    tmp <- getfun(x)
    tmp[[key]] <- collected
    setfun(x, tmp)
}

.clean_internal_names <- function(names, N, msg) {
    if (is.null(names) && N > 0) {
        warning("'", msg, "' is NULL, replacing with 'unnamed'")
        names <- paste0(.unnamed, seq_len(N))
    } else if (any(empty <- names=="")) {
        warning("'", msg, "' contains empty strings, replacing with 'unnamed'")
        names[empty] <- paste0(.unnamed, seq_along(sum(empty)))
    }
    names
}

.get_internal_names <- function(x, getfun, key) {
    x <- updateObject(x)
    colnames(getfun(x)[[key]])
}

.set_internal_names <- function(x, value, getfun, setfun, key) {
    x <- updateObject(x)
    tmp <- getfun(x)
    value <- .clean_internal_names(value, N=ncol(tmp[[key]]), msg="value")
    colnames(tmp[[key]]) <- value
    setfun(x, tmp)
}

.get_internal_missing <- function(x, basefun, namefun, funstr, ...) {
    if (identical(length(namefun(x)), 0L)) {
        stop("no available entries for '", funstr, "(<", class(x), ">, ...)'")
    }
    basefun(x, 1L, ...)
}

.get_internal_integer <- function(x, index, getfun, key, funstr, substr) {
    x <- updateObject(x)
    internals <- getfun(x)[[key]]

    tryCatch({
        internals[, index]
    }, error=function(err) {
        stop("invalid subscript '", substr, "' in '", funstr, "(<", class(x),
             ">, type=\"numeric\", ...)':\n  ", conditionMessage(err))
    })
}

.get_internal_character <- function(x, index, getfun, key, funstr, substr, namestr) {
    x <- updateObject(x)
    internals <- getfun(x)[[key]]

    tryCatch({
        internals[, index]
    }, error=function(err) {
        stop("invalid subscript '", substr, "' in '", funstr, "(<", class(x),
             ">, type=\"character\", ...)':\n  ", "'", index, "' not in '",
             namestr, "(<", class(x), ">)'")
    })
}

.set_internal_missing <- function(x, value, ..., basefun, namefun) {
    if (length(namefun(x))){
        type <- 1L
    } else {
        type <- paste0(.unnamed, 1L)
    }
    basefun(x, type, ..., value=value)
}

.set_internal_numeric <- function(x, type, value, getfun, setfun, key,
    convertfun, xdimfun, vdimfun, funstr, xdimstr, vdimstr, substr)
{
    x <- updateObject(x)

    if (!is.null(value)) {
        if (!is.null(convertfun)) {
            value <- convertfun(value)
        }
        if (!identical(vdimfun(value), xdimfun(x))) {
            stop("invalid 'value' in '", funstr, "(<", class(x),
                 ">, type=\"numeric\") <- value':\n  ",
                "'value' should have number of ", vdimstr, " equal to '",
                xdimstr, "(x)'")
        }
    }

    internals <- getfun(x)
    if (type[1] > ncol(internals[[key]])) {
        stop("'", substr, "' out of bounds in '", funstr, "(<", class(x),
             ">, type='numeric')")
    }

    internals[[key]][[type]] <- value
    setfun(x, internals)
}

.set_internal_character <- function(x, type, value, getfun, setfun, key,
    convertfun, xdimfun, vdimfun, funstr, xdimstr, vdimstr, substr)
{
    x <- updateObject(x)

    if (!is.null(value)) {
        if (!is.null(convertfun)) {
            value <- convertfun(value)
        }
        if (!identical(vdimfun(value), xdimfun(x))) {
            stop("invalid 'value' in '", funstr, "(<", class(x),
                 ">, type=\"character\") <- value':\n  ",
                "'value' should have number of ", vdimstr, " equal to '",
                xdimstr, "(x)'")
        }
    }

    internals <- getfun(x)
    internals[[key]][[type]] <- value
    setfun(x, internals)
}

