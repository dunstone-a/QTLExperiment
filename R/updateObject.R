#' Update a multiStateQTLExperiment object
#'
#' Update \linkS4class{multiStateQTLExperiment} objects to the latest version of
#' the class structure. This is usually called by internal methods rather than
#' by users or downstream packages.
#'
#' @param object A old \linkS4class{multiStateQTLExperiment} object.
#' @param ... Additional arguments that are ignored.
#' @param verbose Logical scalar indicating whether a message should be emitted
#'        as the object is updated.
#'
#' @details
#' This function updates the multiStateQTLExperiment to match changes in the
#' internal class representation. Changes are as follows:
#' \itemize{
#' \item No updates yet.
#' }
#'
#' @return
#' An updated version of \code{object}.
#'
#' @author Christina B Azodi
#'
#' @seealso
#' \code{\link{objectVersion}}, which is used to determine if the object is up-to-date.
#'
#' @name updateObject
#' @export
#' @aliases updateObject updateObject,multiStateQTLExperiment-method
#' @importFrom BiocGenerics updateObject
#' @importFrom S4Vectors DataFrame
#' @importFrom utils packageVersion
setMethod("updateObject", "multiStateQTLExperiment",
          function(object, ..., verbose=FALSE) {
  old.ver <- objectVersion(object)
  triggered <- FALSE

  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }

  # Update possibly outdated DataFrame object.
  object@int_colData <- updateObject(object@int_colData, ..., verbose=verbose)


  if (verbose && triggered) {
    message("[updateObject] ", class(object)[1], " object uses ",
            "internal representation\n", "[updateObject] from multiStateQTLExperiment ",
            old.ver, ". ", "Updating it ...\n", appendLF = FALSE)
  }

  int_metadata(object)$version <- packageVersion("multiStateQTLExperiment")
  #int_metadata(object)$version <- "testing.object.update"
  callNextMethod()
})
