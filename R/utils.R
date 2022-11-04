.convert_subset_index <- function(subset, names) {
  if (is.character(subset)) {
    fmt <- "index out of bounds: %s"
    subset <- SummarizedExperiment:::.SummarizedExperiment.charbound(subset,
                                                                     names, fmt)
  }
  return(as.vector(subset))
}

.unnamed <- "unnamed"

.absent_action <- function(object, val, fun,
                           onAbsence=c("none", "warn", "error")) {
  if (is.null(val)) {
    onAbsence <- match.arg(onAbsence)
    if (onAbsence!="none") {
      msg <- sprintf("'%s(<%s>)' returns 'NULL'", fun, class(object)[1])
      if (onAbsence=="warn") {
        warning(msg)
      } else {
        stop(msg)
      }
    }
  }
}

.absent_file_action <- function(input, onAbsence=c("warn", "error")){

  absent <- input[, "path"]
  absent <- absent[!file.exists(absent)  & !grepl("http", absent)]

  if (length(absent) == length(input$path)){
    stop("No files were found... stopping.")
  } else if (length(absent) > 0){
    msg <- sprintf("The following files are missing: ", absent)
    if (onAbsence=="warn") {
      warning("The following files are missing and will be skipped: ", absent)
      return(input[file.exists(input$file)])
    } else {
      stop("The following files are missing, loading will stop: ", absent)
    }
  } else{
    return(input)
  }

}

.checkSEcolOrder <- function(se){

  for(i in 1:length(assays(se))){
    assay <- names(assays(se))[i]
    if(i == 1){
      col_order <- colnames(assay(se, assay))
      if(is.null(col_order)){
        stop("State IDs should be provided as colnames or state_id={...}.")}
    } else{
      if(!all(colnames(assay(se, assay)) %in% col_order)){
        stop("The assays provided have different state_ids")
      }
      assay(se, assay) <- assay(se, assay)[, col_order]
    }
  }

  return(se)
}


.checkSErowOrder <- function(se){

  for(i in 1:length(assays(se))){
    assay <- names(assays(se))[i]
    if(i == 1){
      row_order <- rownames(assay(se, assay))
      if(is.null(row_order)){
        stop("Feature/variant IDs should be provided as pipe separated string
             in rownames or using feature_id={...} and variant_id={...}.")}
    } else{
      if(!all(rownames(assay(se, assay)) %in% row_order)){
        stop("The assays provided have different state_ids")
      }
      assay(se, assay) <- assay(se, assay)[row_order, ]
    }
  }

  return(se)
}
