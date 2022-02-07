.convert_subset_index <- function(subset, names) {
  if (is.character(subset)) {
    fmt <- "index out of bounds: %s"
    subset <- SummarizedExperiment:::.SummarizedExperiment.charbound(subset, names, fmt)
  }
  return(as.vector(subset))
}

.unnamed <- "unnamed"

.absent_action <- function(object, val, fun, onAbsence=c("none", "warn", "error")) {
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

  absent <- input$file[!file.exists(input$file)]

  if (length(absent) == length(input$file)){
    stop("No files were found... stopping.")
    }

  if(length(absent) != 0){
    msg <- sprintf("The following files are missing: ", absent)
    if (onAbsence=="warn") {
      warning("The following files are missing and will be skipped: ", absent)
      return(input[file.exists(input$file)])
    } else {
      stop("The following files are missing, loading will stop: ", absent)
    }
  }
}

