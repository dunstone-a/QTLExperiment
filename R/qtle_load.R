#' @title Coerce QTL summary statistics into a QTLExperiment object
#'
#' @description
#' A suite of methods to extract QTL mapping summary statistics from common
#' QTL workflow output files.
#'
#' @param input Named array or data.frame with state name and the file to the
#'              QTL summary statistics for that state. If data.frame is
#'              provided, it must include columns 'state' and 'path'. Additional
#'              columns will be stored in the colData annotation.
#' @param feature_id The name/index of the column with the feature_id info.
#' @param variant_id The name/index of the column with the variant_id info.
#' @param betas The name/index of the column with the effect size/beta value.
#' @param errors The name/index of the column with the effect size/beta standard
#'               error value.
#' @param pvalues The name/index of the column with the significance score.
#' @param delayed Logical scalar indicating whether matrices should be wrapped in \linkS4class{DelayedArray}.
#' @param n_max Max number of rows to read per file. This is primarily used
#'              for testing purposes.
#' @param verbose logical. Whether to print progress messages.
#' @param check_dupes logical. Whether to check for duplicate tests for some 
#'        combinations of state, feature ID and variant ID. This is necessary for 
#'        some data (e.g., xQTLatlas) where multiple genetic variants were tested 
#'        for each feature and state. To avoid col_lists in the output object, 
#'        this argument subsets to the first test for each combination. Note that 
#'        checking that tests are unique can slow down run-times so should be avoided
#'        if not necessary. 
#'
#' @example man/examples/qtle_load_example.R
#'
#' @returns A \linkS4class{QTLExperiment} object.
#'
#' @author
#' Christina B Azodi, Amelia Dunstone
#'
#' @export
#' @importFrom vroom vroom
#' @importFrom collapse ftransform fselect fsubset na_omit fmutate qM
#' @importFrom tidyr pivot_wider all_of
#' @importFrom tibble column_to_rownames
#' @importFrom SummarizedExperiment assay
#' @importFrom rlang .data
#'
sumstats2qtle <- function(
        input, feature_id="gene_id", variant_id="variant_pos", betas="slope",
        errors="slope_se", pvalues=NULL, delayed = FALSE, n_max=Inf, verbose=TRUE,
        check_dupes=FALSE){

    path <- state <- id <- NULL

    if(inherits(input, "list")){
        input <- data.frame(list(state=names(input), path=unlist(unname(input))))
    } else if (!all(c("state", "path") %in% colnames(input))){
        warning("input a named array or a df with columns `state` and `path`")
    }

    input <- .absent_file_action(input, onAbsence="warn")

    if(any(endsWith(input$path, '.gz'))){
        warning("vroom will not load all rows in a compressed file.")
    }

    if(is.null(pvalues)){
        data <- vroom(input$path, id="path", show_col_types=FALSE,
            n_max=n_max,
            col_select=list(feature_id=all_of(feature_id),
                variant_id=all_of(variant_id),
                betas=all_of(betas),
                errors=all_of(pvalues)),
            progress=verbose)
    } else{
        data <- vroom(input$path, id="path", show_col_types=FALSE,
            n_max=n_max,
            col_select=list(feature_id=all_of(feature_id),
                variant_id=all_of(variant_id),
                betas=all_of(betas),
                errors=all_of(errors),
                pvalues=all_of(pvalues)),
            progress=verbose)
    }
    
    data$state <- input$state[match(data$path, input$path)]
    data$path <- NULL
    data$id <- paste0(data$feature_id, "|", data$variant_id)

    if (check_dupes) {
        if (any(duplicated(paste0(data$state, data$id)))) {
            warning("Multiple tests present for some combinations of state, feature ID and variant ID. Keeping only first occurences...")
            data <- data |>
                dplyr::distinct(state, id, .keep_all = TRUE)
        }
    }
    
    betas <- data |> 
        pivot_wider(names_from=state, values_from=betas, id_cols=id) |>
        tibble::column_to_rownames(var="id") |> qM()

    errors <- data |> 
        pivot_wider(names_from=state, values_from=errors, id_cols=id) |>
        tibble::column_to_rownames(var="id") |> qM()

    object <- QTLExperiment(
        list(betas=betas, errors=errors),
            feature_id=gsub("\\|.*", "", row.names(betas)),
            variant_id=gsub(".*\\|", "", row.names(betas)))

    colData(object) <- cbind(
        colData(object),
        dplyr::select(input, -dplyr::all_of(c("state", "path"))))

    if(!is.null(pvalues)){
        pvalues <- data |> 
            pivot_wider(names_from=state, values_from=pvalues, id_cols=id) |>
            tibble::column_to_rownames(var="id") |> qM()

        assay(object, "pvalues") <- pvalues
    }

    return(object)
}


