#' @title Coerce QTL summary statistics into a QTLExperiment object
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
#' @param betas The name/index of the column with the effect size/beta value.
#' @param error The name/index of the column with the effect size/beta standard
#'           error value.
#' @param pval The name/index of the column with the significance score.
#' @param n_max Max number of rows to read per file. This is primarily used
#'              for testing purposes.
#' @param verbose logical. Whether to print progress messages.
#'
#' @author
#' Christina B Azodi
#'
#' @export
#' @importFrom vroom vroom
#' @importFrom collapse ftransform fselect fsubset na_omit fmutate qDF
#' @importFrom tidyr pivot_wider all_of
#' @importFrom dplyr left_join %>%
#' @importFrom tibble column_to_rownames
#' @importFrom SummarizedExperiment assay
#' @importFrom rlang .data
#'
summaryStats_2_qtle <- function(input, feature_id = "gene_id",
                                variant_id = "variant_pos", betas = "slope",
                                error = "slope_se", pval = NULL,
                                n_max=Inf, verbose = TRUE){

  path <- state <- id <- NULL

  if(is.list(input)){
    input <- data.frame(list(state=names(input), path=unlist(unname(input))))
  } else if (!all(c("state", "path") %in% colnames(input))){
    warning("input a named array or a df with columns `state` and `path`")
  }

  input <- .absent_file_action(input, onAbsence="warn")

  if(any(endsWith(input$path, '.gz'))){
    warning("vroom will not load all rows in a compressed file.")
  }

  if(is.null(pval)){
    data <- vroom(input$path, id="path", show_col_types = FALSE,
                  n_max = n_max,
                  col_select=list(path, feature_id = all_of(feature_id),
                                  variant_id = all_of(variant_id),
                                  betas = all_of(betas),
                                  error = all_of(error)),
                  progress = verbose)
  } else{
    data <- vroom(input$path, id="path", show_col_types = FALSE,
                  n_max = n_max,
                  col_select=list(path, feature_id = all_of(feature_id),
                                  variant_id = all_of(variant_id),
                                  betas = all_of(betas),
                                  error = all_of(error),
                                  pval = all_of(pval)),
                  progress = verbose)
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

  object <- QTLExperiment(list(betas = betas, error = error),
                          feature_id = gsub("\\|.*", "", row.names(betas)),
                          variant_id = gsub(".*\\|", "", row.names(betas)))

  if(!is.null(pval)){
    pval <- data %>% pivot_wider(names_from = state, values_from = pval,
                                 id_cols = id) %>%
      tibble::column_to_rownames(var = "id") %>% qDF()

    assay(object, "pval") <- pval
  }

  return(object)
}

