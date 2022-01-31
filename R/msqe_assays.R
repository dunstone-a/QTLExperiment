#' @title
#' Named assay getters and setters
#'
#' @description
#' These are methods for getting or setting \code{assay(msqe, i=X, ...)}
#' where \code{msqe} is a \linkS4class{multiStateQTLExperiment} object and
#' \code{X} is the name of the method. For example, \code{betas} will get or
#' set \code{X="betas"}.
#'
#' @section Available methods:
#' Here \code{x} is a \linkS4class{multiStateQTLExperiment} object,
#' \code{value} is a matrix-like object with the same dimensions as \code{x},
#' and \code{...} are further arguments passed to \code{\link{assay}}
#' (for the getter) or \code{\link{assay<-}} (for the setter).
#'
#' \describe{
#' \item{\code{betas(x, ...)}, \code{betas(x, ...) <- value}:}{
#' Get or set a matrix of raw betas, i.e., QTL effect sizes.
#' }
#' \item{\code{se(x, ...)}, \code{se(x, ...) <- value}:}{
#' Get or set a matrix of raw beta standard errors.
#' }
#' \item{\code{sig(x, ...)}, \code{sig(x, ...) <- value}:}{
#' Get or set a matrix of raw significance scores (e.g. pvals, qvals)
#' }
#' \item{\code{adj_betas(x, ...)}, \code{adj_betas(x, ...) <- value}:}{
#' Get or set a matrix of multi-state adjusted betas, i.e., QTL effect sizes
#' adjusted by mashr.
#' }
#' \item{\code{adj_se(x, ...)}, \code{adj_se(x, ...) <- value}:}{
#' Get or set a matrix of multi-state adjusted beta standard errors.
#' }
#' \item{\code{adj_sigl(x, ...)}, \code{adj_sigl(x, ...) <- value}:}{
#' Get or set a matrix of multi-state adjusted significance scores (e.g. lfsr
#' output from mashr)
#' }
#' }
#'
#' @author
#' Christina B Azodi
#'
#' @seealso
#' \code{\link{assay}} and \code{\link{assay<-}}, for the wrapped methods.
#'
#' @examples
#' example(multiStateQTLExperiment, echo=FALSE) # Using the class example
#' betas(msqe) <- matrix(rnorm(nrow(msqe)*ncol(msqe)), ncol=ncol(msqe))
#' dim(betas(msqe))
#'
#' @name msqe-assays
#' @rdname assays
#' @docType methods
#' @aliases
#' betas
#' betas<-
#' betas,multiStateQTLExperiment-method
#' betas<-,multiStateQTLExperiment-method
#' sig
#' sig<-
#' sig,multiStateQTLExperiment-method
#' sig<-,multiStateQTLExperiment-method
#' se
#' se<-
#' se,multiStateQTLExperiment-method
#' se<-,multiStateQTLExperiment-method
#' adj_betas
#' adj_betas<-
#' adj_betas,multiStateQTLExperiment-method
#' adj_betas<-,multiStateQTLExperiment-method
#' adj_sig
#' adj_sig<-
#' adj_sig,multiStateQTLExperiment-method
#' adj_sig<-,multiStateQTLExperiment-method
#' adj_se
#' adj_se<-
#' adj_se,multiStateQTLExperiment-method
#' adj_se<-,multiStateQTLExperiment-method
NULL

GET_FUN <- function(values, ...) {
  (values) # To ensure evaluation
  function(object, ...) {
    assay(object, i=values, ...)
  }
}

SET_FUN <- function(values, ...) {
  (values)
  function(object, ..., value) {
    assay(object, i=values, ...) <- value
    object
  }
}

#' @export
setMethod("betas", "multiStateQTLExperiment", GET_FUN("betas"))

#' @export
setReplaceMethod("betas", c("multiStateQTLExperiment", "ANY"), SET_FUN("betas"))

#' @export
setMethod("error", "multiStateQTLExperiment", GET_FUN("error"))

#' @export
setReplaceMethod("error", c("multiStateQTLExperiment", "ANY"), SET_FUN("error"))

#' @export
setMethod("pval", "multiStateQTLExperiment", GET_FUN("pval"))

#' @export
setReplaceMethod("pval", c("multiStateQTLExperiment", "ANY"), SET_FUN("pval"))

#' @export
setMethod("adj_betas", "multiStateQTLExperiment", GET_FUN("adj_betas"))

#' @export
setReplaceMethod("adj_betas", c("multiStateQTLExperiment", "ANY"), SET_FUN("adj_betas"))

#' @export
setMethod("adj_error", "multiStateQTLExperiment", GET_FUN("adj_error"))

#' @export
setReplaceMethod("adj_error", c("multiStateQTLExperiment", "ANY"), SET_FUN("adj_error"))

#' @export
setMethod("adj_pval", "multiStateQTLExperiment", GET_FUN("adj_pval"))

#' @export
setReplaceMethod("adj_pval", c("multiStateQTLExperiment", "ANY"), SET_FUN("adj_pval"))

#' @export
setMethod("lfsr", "multiStateQTLExperiment", GET_FUN("lfsr"))

#' @export
setReplaceMethod("lfsr", c("multiStateQTLExperiment", "ANY"), SET_FUN("lfsr"))

