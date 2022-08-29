########################################
# Getter/setters for feature_id, variant_id, and state

#' @export
setGeneric("feature_id", function(object, ...) standardGeneric("feature_id"))

#' @export
setGeneric("feature_id<-", function(object, ..., value) standardGeneric("feature_id<-"))

#' @export
setGeneric("variant_id", function(object, ...) standardGeneric("variant_id"))

#' @export
setGeneric("variant_id<-", function(object, ..., value) standardGeneric("variant_id<-"))

#' @export
setGeneric("state_id", function(object, ...) standardGeneric("state_id"))

#' @export
setGeneric("state_id<-", function(object, ..., value) standardGeneric("state_id<-"))

########################################
# Hidden getter/setters for internal slots.

#' @export
setGeneric("int_rowData", function(x) standardGeneric("int_rowData"))

#' @export
setGeneric("int_rowData<-", function(x, value) standardGeneric("int_rowData<-"))

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
setGeneric("objectVersion", function(x) standardGeneric("objectVersion"))

#' @export
setGeneric("mainExpName", function(x) standardGeneric("mainExpName"))

#' @export
setGeneric("mainExpName<-", function(x, value) standardGeneric("mainExpName<-"))

#' @export
setGeneric("colLabels", function(x, ...) standardGeneric("colLabels"))

#' @export
setGeneric("colLabels<-", function(x, ..., value) standardGeneric("colLabels<-"))


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
setGeneric("lfsr", function(object, ...) standardGeneric("lfsr"))

#' @export
setGeneric("lfsr<-", function(object, ..., value) standardGeneric("lfsr<-"))


