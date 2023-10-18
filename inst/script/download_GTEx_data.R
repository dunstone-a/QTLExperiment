# Download data from EBI eQTL database
# Amelia Dunstone

# This script downloads raw data from the EBI eQTL database.
# Since each of these files is very large (~1GB), this script subsets the data
# to provide sample data for the vignette for the package 'QTLExperiment'.

# Website: https://www.ebi.ac.uk/eqtl/Data_access/
# Data is licensed under the Creative Commons license, https://creativecommons.org/licenses/by/4.0/

# Obtain URLs ------------------------------------------------------------------

file_path <- "inst/extdata/"

# File containing metadata for all uniformly processed datasets
# from the EBI eQTL database (second hyperlink on this page: https://www.ebi.ac.uk/eqtl/Data_access/)
# GitHub url: https://raw.githubusercontent.com/eQTL-Catalogue/eQTL-Catalogue-resources/master/data_tables/dataset_metadata.tsv
ebi <- read.csv(paste0(file_path, "/EBI_dataset_metadata.tsv"), sep="\t")

# URL for head of the data files
ftp <- "http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats"

# Filter to select the tissue and make url's
input <- ebi |>
    dplyr::filter(study_label == "GTEx") |>
    dplyr::filter(tissue_label %in% c("lung", "thyroid", "spleen", "blood")) |>
    dplyr::filter(quant_method == "tx") |>
    dplyr::mutate(path=paste(ftp, study_id, dataset_id, dataset_id, sep="/")) |>
    dplyr::mutate(path=paste0(path, ".cc.tsv.gz")) |>
    dplyr::rename(state=tissue_label) |>
    dplyr::select(state, path)


# Download raw data ------------------------------------------------------------

library(vroom)
# vroom will not load all rows in a compressed file.

for (i in seq_along(input$state)) {

    message(i)
    data <- vroom(
        input$path[i],
        show_col_types=FALSE,
        n_max=1000,
        progress=TRUE)

    write.table(
        data,
        file=paste0(file_path, "GTEx_tx_", input$state[i], ".tsv"),
        sep="\t",
        row.names=FALSE)

}

# Test that sumstats2qtl works with this input ---------------------------------

# For testing only:
# feature_id <- "molecular_trait_id"
# variant_id <- "rsid"
# betas <- "beta"
# errors <- "se"
# pvalues <- "pvalue"
# n_max <- Inf
# verbose <- TRUE

input_path <- system.file("extdata", package= "QTLExperiment")
state <- c("lung", "thyroid", "spleen", "blood")

input <- data.frame(
    state=state,
    path=paste0(input_path, "/GTEx_tx_", state, ".tsv"))

qtle4 <- sumstats2qtle(
    input,
    feature_id="molecular_trait_id",
    variant_id="rsid",
    betas="beta",
    errors="se",
    pvalues="pvalue",
    verbose=TRUE)
qtle4

