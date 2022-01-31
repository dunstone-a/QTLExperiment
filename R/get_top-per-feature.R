#' Function to subset get top QTL for each feature
#'
#' This function pulls the top QTL for each feature (e.g., gene) and
#' outputs a **new** \linkS4class{multiStateQTLExperiment}. Note that the
#' output from this function should be assigned to a new variable as it will
#' overwrite if assigned to the same variable.
#'
#' @param object A \linkS4class{multiStateQTLExperiment} object with multiple
#'        QTL tests (i.e., rows) for at least one feature.
#' @param feature Name of feature id in colData(object).
#' @param assay Assay to use as filter assay
#' @param keep min/max to keep the test for each feature with the min or the
#'        max value in the assay.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom data.table setDT
#' @importFrom SummarizedExperiment assay
#' @export
#'
#'
subsetPerFeature <- function(object, assay = "pval", FUN = which.min) {

  feature_id <- rowData(object)$featureID
  var_id <- rowData(object)$snpID

  top_hits <- apply(assay(object, assay), 2,
                    function(x) .getTopPerFeature(x, feature_id, var_id, FUN))

  rowData(object)$snpID <- NULL

  outList <- list()
  for(s in colnames(object)){
    tmp <- object[top_hits[, s], s] #
    rownames(tmp) <- rowData(tmp)$featureID
    outList[[s]] <- tmp
  }

  out <- do.call(cbind, outList)
  metadata(out) <- metadata(out)[!duplicated(metadata(out))]
  top_hits <- gsub(".*\\|", "", top_hits)
  colnames(top_hits) <- paste0("top-eSNP_", colnames(top_hits))
  rowData(out) <- cbind(rowData(out), top_hits)

  return(out)
}

#' @importFrom data.table setDT .SD
.getTopPerFeature <- function(x, by, var, FUN){
  tmp <- setDT(list(x=x, by=by, var=var))
  tmp <- tmp[, .SD[FUN(x)], by = by]
  return(paste0(tmp$by, "|", tmp$var))
}
