#' # Internal QTLExperiment functions
#' @returns For \code{int_metadata}, returns the value stored in the requested field 
#' of the internal rowData, colData or metaData.
#' 
#'  For \code{int_metadata<-value}, the relevant internal field of the \linkS4class{QTLExperiment} 
#'  is updated.
#'  
#' @noRd
#' @author Christina B Azodi
NULL

#########################################
### Defining methods for int_metadata ###
#########################################


setMethod("int_metadata", "QTLExperiment", function(object) object@int_metadata)

setReplaceMethod("int_metadata", "QTLExperiment", function(object, value) {
    object@int_metadata <- value
    return(object)
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

        names(value) <- .clean_internal_names(names(value), N=length(value),
                                              msg="names(value)")
        collected <- do.call(DataFrame, c(lapply(value, I),
                                          list(row.names=NULL,
                                               check.names=FALSE)))

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
        names <- paste0(.unnamed, seq(N))
    } else if (any(empty <- names=="")) {
        warning("'", msg, "' contains empty strings, replacing with 'unnamed'")
        names[empty] <- paste0(.unnamed, seq(sum(empty)))
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


