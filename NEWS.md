## Changes in version 2.1.3 (2025-10-27)
* Changed the data loading function sumstats2qtle().
    - Added a flag to skip checking for multiple association tests for combinations of state, feature and variant. 
    - Replaced a left_join with a match which should be faster. 

## Changes in version 2.1.2 (2025-09-30)
* Added support for QTLExperiment objects with only one state, i.e. one column. 
    - sumstats2qtle() can read in data when input has only one path.
    - mockQTLE() allows nStates = 1. 

## Changes in version 2.1.1 (2025-07-14)
* subset function is now a generic.

## Changes in version 1.99.3 (2025-05-26)
* Fixed sumstats2qtle function so that the betas, errors and pvalues are matrices and not data.frames.

## Changes in version 1.99.2 (2025-04-11)
*  Updated the function sumstats2qtle to print a warning when there are duplicate row names. 

## Changes in version 1.99.1 (2025-03-05)
*  Major update to the underlying representation of the slots. 
    -  Slots now use elementMetadata and colData. 
    -  No longer use internal col and row Data. 
    -  Almost all functions have been updated and testing is working but there may still be some edge cases that I have missed. 
    -  Better consistency with other child classes of SingleCellExperiment.

## Changes in version 1.5.0 (2024-10-30)
*  Added a test for object validity error messages

## Changes in version 1.3.0 (2024-05-01)
*  Minor changes to tests
*  Typo and indentation in vignette

## Changes in version 0.99.6 (2023-10-19) 
*  Added return statements for all documentation files. This removes the warning from BiocCheck()

## Changes in version 0.99.5 (2023-10-19) 
*  Added a few tests for internal functions

## Changes in version 0.99.4 (2023-10-18)
*  Updated description and readme
*  Improved indentation consistency
*  Split the miscellaneous documentation file into two

## Changes in version 0.99.3 (2023-10-10)
*  Removed all external downloads by bundling data into package

## Changes in version 0.99.2 (2023-10-03)
* Added testing during R CMD check
* Added more examples and updated documentation
* Added support for additional colData in sumstats2QTL()   

## Changes in version 0.99.1 (2023-09-28)
* Small tweaks e.g. using seq_along()

## Changes in version 0.99.0 (2023-09-28)
* Submitted to Bioconductor
    
