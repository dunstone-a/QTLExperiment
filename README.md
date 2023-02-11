# QTLExperiment Package

QTLExperiment is an R container package for storing and manipulating QTL summary statistics. 

|                |               |
| -------------- | ------------- |
| Project Status | [![Project Status.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) |
| Latest release  | 2022-11-08 |


## Installation and Usage

This package in stable but undergoing active development and currently only lives on GitHub. To install from GitHub, use devtools:

```
install.packages("devtools")
devtools::install_git("https://gitlab.svi.edu.au/biocellgen-public/qtlexperiment.git", build_vignettes = TRUE)
```

We plan to submit QTLExperiment and multistateQTL to Bioconductor in the near future. Using the most recent version of R is strongly recommended (R 4.2.1 at the time of writing). 

There are several other packages from CRAN and Bioconductor that QTLExperiment uses, so you will need to have these packages installed as well. The CRAN packages should install automatically when QTLExperiment is installed, but you will need to install the Bioconductor packages manually.

Not all of the following are strictly necessary, but have been included here as they enhance the functionality of QTLExperiment. The commands below should help with package installations.

### CRAN

```{r install-cran}
install.packages(c("knitr", "dplyr", "collapse", "vroom", "ashr", "tidyr", "testthat"))
```

### Bioconductor 

```{r load-bioc}
source("http://bioconductor.org/biocLite.R")
biocLite(c("SummarizedExperiment", "BiocGenerics", "S4Vectors"))
```

## Getting started

Get started with QTLExperiment by checking out the vignette that introduces the main features and functionality of QTLExperiment. From inside an R session, load QTLExperiment and then browse the vignettes:

```
library(QTLExperiment)
browseVignettes("QTLExperiment")
```



## Acknowledgements and disclaimer
The package leans heavily on previously published work and packages, namely SummarizedExperiment. The QTLE S4 class is inspired by SingleCellExperiment. 

The package is currently in a Beta state. The major functionality of the package is settled, but it is still under development so may change from time to time. Please do try it and contact me with bug reports, feedback, feature requests, questions and suggestions to improve the package.

Christina Brady Del Azodi, November 2022
