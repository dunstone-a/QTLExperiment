#' @title Coerce QTL summary statistics into a multiStateQTLExperiment object
#'
#' @description
#' A suite of methods to extract QTL mapping summary statistics from common
#' QTL workflow output files.
#'
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, additional columns will be stored in the colData
#'              annotation.
#' @param feature_id The name/index of the column with the feature_id info.
#' @param variant_id The name/index of the column with the variant_id info.
#' @param beta The name/index of the column with the effect size/beta value.
#' @param error The name/index of the column with the effect size/beta standard
#'           error value.
#' @param pval The name/index of the column with the significance score.
#' @param na.rm Logical. To remove QTL tests with missing data for any state.
#' @param n_max Max number of rows to read per file. This is primarily used
#'              for testing purposes.
#' @param verbose logical. Whether to print progress messages.
#'
#' @author
#' Christina B Azodi
#'
#' @export
#' @importFrom vroom vroom
#' @importFrom collapse ftransform fselect fsubset na_omit
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join %>%
#' @importFrom tibble column_to_rownames
#' @importFrom SummarizedExperiment assay
#'
summaryStats_2_msqe <- function(input, feature_id = "gene_id",
                                variant_id = "variant_pos", betas = "slope",
                                error = "slope_se", pval = "pval_perm",
                                na.rm = FALSE, n_max=Inf, verbose = TRUE){

  if(class(input)=="list"){
    input <- data.frame(list(state=names(input), path=unlist(unname(input))))
  } else if (!all(c("state", "path") %in% colnames(input))){
    error("input a named array or a df with columns `state` and `path`")
  }

  input <- .absent_file_action(input, onAbsence="warn")

  if(is.null(pval)){
    data <- vroom(input$path, id="path", show_col_types = FALSE,
                col_select=list(path, feature_id = all_of(feature_id),
                                variant_id = all_of(variant_id),
                                betas = all_of(betas),
                                error = all_of(error)),
                progress = verbose, n_max = n_max)
  } else{
    data <- vroom(input$path, id="path", show_col_types = FALSE,
                  col_select=list(path, feature_id = all_of(feature_id),
                                  variant_id = all_of(variant_id),
                                  betas = all_of(betas),
                                  error = all_of(error),
                                  pval = all_of(pval)),
                  progress = verbose, n_max = n_max)
  }

  data <- data %>% left_join(input, "path") %>%
    fselect(-path) %>%
    fmutate(id = paste0(feature_id, "|", variant_id))

  betas <- data %>% pivot_wider(names_from = state, values_from = betas,
                                id_cols = id) %>%
    tibble::column_to_rownames(var = "id") %>% qDF()

  error <- data %>% pivot_wider(names_from = state, values_from = error,
                                id_cols = id) %>%
    tibble::column_to_rownames(var = "id") %>% qDF()

  rowData <- DataFrame(list(feature_id = gsub("\\|.*", "", row.names(betas)),
                            variant_id = gsub(".*\\|", "", row.names(betas))))

  msqe <- multiStateQTLExperiment(list(betas = betas, error = error),
                                  rowData=rowData)

  if(!is.null(pval)){
    pval <- data %>% pivot_wider(names_from = state, values_from = pval,
                                 id_cols = id) %>%
      tibble::column_to_rownames(var = "id") %>% qDF()

    assay(msqe, "pval") <- pval
  }

  if(na.rm) { msqe <- subsetComplete(msqe, 1, verbose) }

  return(msqe)

}


