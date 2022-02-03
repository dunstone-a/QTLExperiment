
########################################
# Getter/setters for reducedDim.

#' @export
setGeneric("reducedDim", function(x, type, ...) standardGeneric("reducedDim"))

#' @export
setGeneric("reducedDim<-", function(x, type, withDimnames=TRUE, ..., value) standardGeneric("reducedDim<-"))

#' @export
setGeneric("reducedDimNames", function(x) standardGeneric("reducedDimNames"))

#' @export
setGeneric("reducedDimNames<-", function(x, value) standardGeneric("reducedDimNames<-"))

#' @export
setGeneric("reducedDims", function(x, ...) standardGeneric("reducedDims"))

#' @export
setGeneric("reducedDims<-", function(x, withDimnames=TRUE, ..., value) standardGeneric("reducedDims<-"))

########################################
# Hidden getter/setters for internal slots.

#' @export
setGeneric("int_elementMetadata", function(x) standardGeneric("int_elementMetadata"))

#' @export
setGeneric("int_elementMetadata<-", function(x, value) standardGeneric("int_elementMetadata<-"))

#' @export
setGeneric("int_colData", function(x) standardGeneric("int_colData"))

#' @export
setGeneric("int_colData<-", function(x, value) standardGeneric("int_colData<-"))

#' @export
setGeneric("int_metadata", function(x) standardGeneric("int_metadata"))

#' @export
setGeneric("int_metadata<-", function(x, value) standardGeneric("int_metadata<-"))

########################################
# Miscellaneous methods.

#' @export
setGeneric("colLabels", function(x, ...) standardGeneric("colLabels"))

#' @export
setGeneric("colLabels<-", function(x, ..., value) standardGeneric("colLabels<-"))

#' @export
setGeneric("rowSubset", function(x, ...) standardGeneric("rowSubset"))

#' @export
setGeneric("rowSubset<-", function(x, ..., value) standardGeneric("rowSubset<-"))

#' @export
setGeneric("objectVersion", function(x) standardGeneric("objectVersion"))

########################################

#' @export
setGeneric("mainExpName", function(x) standardGeneric("mainExpName"))

#' @export
setGeneric("mainExpName<-", function(x, value) standardGeneric("mainExpName<-"))


########################################
# Convenience assay getter/setters.

#' @export
setGeneric("betas", function(object, ...) standardGeneric("betas"))

#' @export
setGeneric("betas<-", function(object, ..., value) standardGeneric("betas<-"))

#' @export
setGeneric("error", function(object, ...) standardGeneric("error"))

#' @export
setGeneric("error<-", function(object, ..., value) standardGeneric("error<-"))

#' @export
setGeneric("pval", function(object, ...) standardGeneric("pval"))

#' @export
setGeneric("pval<-", function(object, ..., value) standardGeneric("pval<-"))

#' @export
setGeneric("adj_betas", function(object, ...) standardGeneric("adj_betas"))

#' @export
setGeneric("adj_betas<-", function(object, ..., value) standardGeneric("adj_betas<-"))

#' @export
setGeneric("adj_error", function(object, ...) standardGeneric("adj_error"))

#' @export
setGeneric("adj_error<-", function(object, ..., value) standardGeneric("adj_error<-"))

#' @export
setGeneric("adj_pval", function(object, ...) standardGeneric("adj_pval"))

#' @export
setGeneric("adj_pval<-", function(object, ..., value) standardGeneric("adj_pval<-"))

#' @export
setGeneric("lfsr", function(object, ...) standardGeneric("lfsr"))

#' @export
setGeneric("lfsr<-", function(object, ..., value) standardGeneric("lfsr<-"))


