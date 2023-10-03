
input_path <- system.file("extdata", package =  "QTLExperiment")
state <- c("lung", "thyroid", "spleen", "blood")

# Input as a named array
input_list <- list(lung = paste0(input_path, "/GTEx_tx_lung.tsv"),
                   spleen = paste0(input_path, "/GTEx_tx_spleen.tsv"))

# Input as a data.frame.
# Must include columns 'state' and 'path'.
input_df <- data.frame(state = c("lung", "spleen"),
                       path = c(paste0(input_path, "/GTEx_tx_lung.tsv"),
                                paste0(input_path, "/GTEx_tx_spleen.tsv")))

# List version
qtle1 <- sumstats2qtle(input_list,
                       feature_id="molecular_trait_id",
                       variant_id="rsid",
                       betas="beta",
                       errors="se",
                       pvalues="pvalue",
                       verbose=TRUE)
qtle1
head(betas(qtle1))

# data.frame version
qtle2 <- sumstats2qtle(input_df,
                       feature_id="molecular_trait_id",
                       variant_id="rsid",
                       betas="beta",
                       errors="se",
                       pvalues="pvalue",
                       verbose=TRUE)
qtle2
head(betas(qtle2))


