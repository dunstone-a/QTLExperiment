ftp <- "http://ftp.ebi.ac.uk/pub/databases/spot/eQTL/sumstats"

# Input as a named array
input_list <- list(lung = paste0(ftp, "/QTS000015/QTD000273/QTD000273.cc.tsv.gz"),
                   spleen = paste0(ftp, "/QTS000015/QTD000328/QTD000328.cc.tsv.gz"))

# Input as a data.frame.
# Must include columns 'state' and 'path'.
input_df <- data.frame(state = c("lung", "spleen"),
                       path = c(paste0(ftp, "/QTS000015/QTD000273/QTD000273.cc.tsv.gz"),
                                paste0(ftp, "/QTS000015/QTD000328/QTD000328.cc.tsv.gz")))

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


