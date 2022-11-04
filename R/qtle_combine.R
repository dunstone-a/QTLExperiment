#' @title Combining QTLExperiment objects
#'
#' @description
#' An overview of methods to combine multiple
#' \linkS4class{QTLExperiment} objects by row or column.
#' These methods ensure that all data fields remain synchronized when states
#' or associations are added or removed.
#'
#' @section Combining:
#' In the following examples, \code{...} contains one or more
#' \linkS4class{QTLExperiment} object.
#' \describe{
#' \item{\code{rbind(..., deparse.level=1)}:}{Returns a
#' \linkS4class{QTLExperiment} object where all objects are combined
#' row-wise. Metadata is combined as in
#' \code{?"\link{rbind,SummarizedExperiment-method}"}. The \code{deparse.level}
#' specifies how row.names are generated as described in
#' \code{?\link[base]{rbind}}.
#' }
#'
#' \item{\code{cbind(..., deparse.level=1)}:}{Returns a
#' \linkS4class{QTLExperiment} object where all objects are combined
#' column-wise. Metadata is combined
#' as in \code{?"\link{cbind,SummarizedExperiment-method}"}. The
#' \code{deparse.level} specifies how colnames are generated as described in
#' \code{?\link[base]{cbind}}.
#' }
#' }
#'
#' @author
#' Christina B Azodi
#'
#' @examples
#' qtle <- mockQTLE()
#' qtle2 <- qtle
#' feature_id(qtle2) <- paste0("x", feature_id(qtle2))
#' rbind(qtle, qtle2)
#'
#' qtle2 <- qtle
#' state_id(qtle2) <- paste0("x", state_id(qtle2))
#' cbind(qtle, qtle2)
#'
#'
#' @docType methods
#' @aliases
#' cbind,QTLExperiment-method
#' rbind,QTLExperiment-method
#'
#' @name QTLe-combine
#' @rdname combine
NULL

#' @export
#' @importFrom BiocGenerics rbind cbind
#'
setMethod("cbind", "QTLExperiment", function(..., deparse.level=1) {
  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }
  out <- callNextMethod()

  args <- list(...)
  args <- lapply(args, updateObject)

  int_meta <- do.call(c, lapply(args, int_metadata))

  tryCatch({
    int_colD <- do.call(rbind, lapply(args, int_colData))
  }, error=function(err) {
    stop(
      "failed to combine 'int_colData' in 'cbind(<", class(args[[1]]), ">)':\n",
      conditionMessage(err))
  })

  # Creating a shell to avoid having to pull out .cbind.DataFrame
  # to fuse metadata along the dimension not being combined.
  row_shells <- lapply(args, .create_shell_rowdata)
  tryCatch({
    combined <- do.call(SummarizedExperiment::cbind, row_shells)
  }, error=function(err) {
    stop( "failed to combine 'int_rowData' in 'cbind(<",
          class(args[[1]]), ">)':\n  ", conditionMessage(err))
  })
  int_eleMetaD <- rowData(combined)

  out <- BiocGenerics:::replaceSlots(out, int_colData=int_colD,
                              int_rowData=int_eleMetaD,
                              int_metadata=int_meta, check=FALSE)

  recover_qtle_ids(out)
})

#' @export
#' @importFrom BiocGenerics rbind cbind
setMethod("rbind", "QTLExperiment", function(..., deparse.level=1) {
  old <- S4Vectors:::disableValidity()
  if (!isTRUE(old)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old))
  }
  out <- callNextMethod()

  args <- list(...)
  args <- lapply(args, updateObject)
  int_meta <- do.call(c, unname(lapply(args, int_metadata)))

  tryCatch({
    int_eleMetaD <- do.call(rbind, lapply(args, int_rowData))
  }, error=function(err) {
    stop("failed to combine 'int_rowData' in 'rbind(<",
         class(args[[1]]), ">)':\n  ", conditionMessage(err))
  })

  # Creating a shell to avoid having to pull out .cbind.DataFrame
  # to fuse metadata along the dimension not being combined.
  col_shells <- lapply(args, .create_shell_coldata)
  tryCatch({
    combined <- do.call(SummarizedExperiment::rbind, col_shells)
  }, error=function(err) {
    stop("failed to combine 'int_colData' in 'rbind(<", class(args[[1]]),
         ">)'\n", conditionMessage(err))
  })
  int_colD <- colData(combined)


  out <- BiocGenerics:::replaceSlots(out, int_colData=int_colD,
                              int_rowData=int_eleMetaD,
                              int_metadata=int_meta, check=FALSE)
  recover_qtle_ids(out)
})

#' @importFrom SummarizedExperiment SummarizedExperiment
.create_shell_coldata <- function(x) {
  SummarizedExperiment(colData=int_colData(x))
}

#' @importFrom SummarizedExperiment SummarizedExperiment
.create_shell_rowdata <- function(x) {
  SummarizedExperiment(rowData=int_rowData(x))
}

