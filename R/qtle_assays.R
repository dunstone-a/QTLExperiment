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
#' \item{\code{errors(x, ...)}, \code{errors(x, ...) <- value}:}{
#' Get or set a matrix of raw beta standard errors.
#' }
#' \item{\code{pvalues(x, ...)}, \code{pvalues(x, ...) <- value}:}{
#' Get or set a matrix of raw significance scores (e.g. pvals, qvals)
#' }
#' \item{\code{lfsrs(x, ...)}, \code{lfsrs(x, ...) <- value}:}{
#' Get or set a matrix of local false sign rates.
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
#' qtle <- mockQTLE()
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
#' errors
#' errors<-
#' errors,QTLExperiment-method
#' errors<-,QTLExperiment-method
#' pvalues
#' pvalues<-
#' pvalues,QTLExperiment-method
#' pvalues<-,QTLExperiment-method
#' lfsrs
#' lfsrs<-
#' lfsrs,QTLExperiment-method
#' lfsrs<-,QTLExperiment-method
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
setMethod("errors", "QTLExperiment", GET_FUN("errors"))

#' @export
setReplaceMethod("errors", c("QTLExperiment", "ANY"), SET_FUN("errors"))

#' @export
setMethod("pvalues", "QTLExperiment", GET_FUN("pvalues"))

#' @export
setReplaceMethod("pvalues", c("QTLExperiment", "ANY"), SET_FUN("pvalues"))

#' @export
setMethod("lfsrs", "QTLExperiment", GET_FUN("lfsrs"))

#' @export
setReplaceMethod("lfsrs", c("QTLExperiment", "ANY"), SET_FUN("lfsrs"))

