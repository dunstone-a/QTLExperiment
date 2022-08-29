#' @title
#' Named assay getters and setters
#'
#' @description
#' These are methods for getting or setting \code{assay(qtle, i=X, ...)}
#' where \code{qtle} is a \linkS4class{QTLExperiment} object and
#' \code{X} is the name of the method. For example, \code{betas} will get or
#' set \code{X="betas"}.
#'
#' @section Available methods:
#' Here \code{x} is a \linkS4class{QTLExperiment} object,
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
#' qtle <- mockQTLe()
#' new_betas <- matrix(rnorm(nrow(qtle)*ncol(qtle)), ncol=ncol(qtle))
#' row.names(new_betas) <- row.names(qtle)
#' colnames(new_betas) <- colnames(qtle)
#' betas(qtle) <- new_betas
#' dim(betas(qtle))
#'
#' @name qtle-assays
#' @rdname assays
#' @docType methods
#' @aliases
#' betas
#' betas<-
#' betas,QTLExperiment-method
#' betas<-,QTLExperiment-method
#' error
#' error<-
#' error,QTLExperiment-method
#' error<-,QTLExperiment-method
#' pval
#' pval<-
#' pval,QTLExperiment-method
#' pval<-,QTLExperiment-method
#' lfsr
#' lfsr<-
#' lfsr,QTLExperiment-method
#' lfsr<-,QTLExperiment-method
#'
NULL

#' @importFrom SummarizedExperiment assay
#'
GET_FUN <- function(values, ...) {
  (values) # To ensure evaluation
  function(object, ...) {
    assay(object, i=values, ...)
  }
}

#' @importFrom SummarizedExperiment assay<-
#'
SET_FUN <- function(values, ...) {
  (values)
  function(object, ..., value) {
    assay(object, i=values, ...) <- value
    object
  }
}

#' @export
setMethod("betas", "QTLExperiment", GET_FUN("betas"))

#' @export
setReplaceMethod("betas", c("QTLExperiment", "ANY"), SET_FUN("betas"))

#' @export
setMethod("error", "QTLExperiment", GET_FUN("error"))

#' @export
setReplaceMethod("error", c("QTLExperiment", "ANY"), SET_FUN("error"))

#' @export
setMethod("pval", "QTLExperiment", GET_FUN("pval"))

#' @export
setReplaceMethod("pval", c("QTLExperiment", "ANY"), SET_FUN("pval"))

#' @export
setMethod("lfsr", "QTLExperiment", GET_FUN("lfsr"))

#' @export
setReplaceMethod("lfsr", c("QTLExperiment", "ANY"), SET_FUN("lfsr"))

