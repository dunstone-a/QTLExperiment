#' Get or set column labels
#'
#' Get or set column labels in a \linkS4class{multiStateQTLExperiment} instance
#' that represent information about the states.
#'
#' @param x A \linkS4class{multiStateQTLExperiment} object.
#' @param value Any vector-like object of \code{ncol(object)} length containing
#' labels for all states. Or set to \code{NULL} to remove existing labels.
#' @param ... Additional arguments, currently ignored.
#' @param onAbsence String indicating an additional action to take when labels are absent:
#' nothing (\code{"none"}), a warning (\code{"warn"}) or an error (\code{"error"}).
#'
#' @details
#' Information about states can be easily stored and retrieved in the
#' \code{\link{colData}} compartment using standard methods
#' (e.g., \code{x$label <- my.labels}).
#'
#' The \code{colLabels} function will get or set a user specified identifier for
#' each state from the \code{"label"} field of the \code{\link{colData}}.
#'
#' This considers the use case where there is a \dQuote{primary} set of labels
#' that represents the default grouping of states in downstream analyses.
#' For example, if a downstream function accepts a `multiStateQTLExperiment`
#' object and requires labels, \code{colLabels(x)} is set as the default value
#' for our label argument. This allows for changes to a \dQuote{secondary} set
#' of labels in \code{x} without setting \code{colLabels(x) <- second.labels},
#' while facilitating convenient use of the primary labels by default.
#'
#' For developers, \code{onAbsence} is provided to make it easier to mandate
#' that \code{x} actually has labels. This avoids silent \code{NULL} values
#' that flow to the rest of the function and make debugging difficult.
#'
#' @author Christina Azodi
#'
#' @return
#' For \code{colLabels}, a vector or equivalent is returned containing label
#' assignments for all states.
#' If no labels are available, a \code{NULL} is returned (and/or a warning or
#' error, depending on \code{onAbsence}).
#'
#' For \code{colLabels<-}, a modified \code{x} is returned with labels in its
#' \code{\link{colData}}.
#'
#' @seealso
#' \linkS4class{multiStateQTLExperiment}, for the underlying class definition.
#'
#' @examples
#' msqe <- mockMSQE()
#' colLabels(msqe) <- sample(LETTERS, ncol(msqe), replace=TRUE)
#' colLabels(msqe)
#'
#' @docType methods
#' @name colLabels
#' @aliases colLabels colLabels<-
NULL

.label_field <- "label"

#' @export
#' @rdname colLabels
#' @importFrom SummarizedExperiment colData
setMethod("colLabels", "multiStateQTLExperiment", function(x, onAbsence="none") {
  output <- colData(x)[[.label_field]]
  .absent_action(x, val=output, fun="colLabels", onAbsence=onAbsence)
  output
})

#' @export
#' @rdname colLabels
#' @importFrom SummarizedExperiment colData<- colData
setReplaceMethod("colLabels", "multiStateQTLExperiment", function(x, ..., value) {
  colData(x)[[.label_field]] <- value
  x
})



