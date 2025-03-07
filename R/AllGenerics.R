########################################
# Getter/setters for feature_id, variant_id, and state

#' @export
setGeneric("feature_id", function(object) standardGeneric("feature_id"))

#' @export
setGeneric("feature_id<-", function(object, value) standardGeneric("feature_id<-"))

#' @export
setGeneric("variant_id", function(object) standardGeneric("variant_id"))

#' @export
setGeneric("variant_id<-", function(object, value) standardGeneric("variant_id<-"))

#' @export
setGeneric("state_id", function(object) standardGeneric("state_id"))

#' @export
setGeneric("state_id<-", function(object, value) standardGeneric("state_id<-"))

########################################
# Hidden getter/setters for internal slots.


setGeneric("int_metadata", function(object) standardGeneric("int_metadata"))

setGeneric("int_metadata<-", function(object, value) standardGeneric("int_metadata<-"))

########################################
# Miscellaneous methods.

#' @export
setGeneric("objectVersion", function(object) standardGeneric("objectVersion"))

#' @export
setGeneric("mainExpName", function(object) standardGeneric("mainExpName"))

#' @export
setGeneric("mainExpName<-", function(object, value) standardGeneric("mainExpName<-"))



########################################
# Convenience assay getter/setters.

#' @export
setGeneric("betas", function(object, ...) standardGeneric("betas"))

#' @export
setGeneric("betas<-", function(object, ..., value) standardGeneric("betas<-"))

#' @export
setGeneric("errors", function(object, ...) standardGeneric("errors"))

#' @export
setGeneric("errors<-", function(object, ..., value) standardGeneric("errors<-"))

#' @export
setGeneric("pvalues", function(object, ...) standardGeneric("pvalues"))

#' @export
setGeneric("pvalues<-", function(object, ..., value) standardGeneric("pvalues<-"))

#' @export
setGeneric("lfsrs", function(object, ...) standardGeneric("lfsrs"))

#' @export
setGeneric("lfsrs<-", function(object, ..., value) standardGeneric("lfsrs<-"))


