
# Impulsivity as a trait in domestic dogs (_Canis familiaris_): A systematic review and meta-analysis

-   Created on 2022-12-22 by Jeffrey R. Stevens
    (<jeffrey.r.stevens@gmail.com>)
-   Finalized on 2023-01-05

This repository provides the reproducible research materials for our project that provides a systematic review of impulsivity as a trait in dogs. This includes the following:

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript
-   R Markdown file for supplementary materials

## Citation

If you use any of these materials, please cite:

Barela, J., Worth, Y., & Stevens, J.R. (2023). Impulsivity as a trait in domestic dogs (Canis familiaris): A systematic review and meta-analysis https://doi.org/10.31234/osf.io/ctfns.

## Summary

One data file (`barela_etal_2023_data1.xlsx`) contains all records from PsychINFO, Scopus, and Web of Science collected on 2022-06-18. Each sources has it's own sheet in the Excel file, and they include all of the metadata from each record. Each row represents a record. The second data file (`barela_etal_2023_data2.csv`) comprises the 64 reviewed studies and includes study metadata as well as effect size information for analyzed studies. Each row represents a study (reports could have more than one study).

## License

All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any     reasonable manner, but not in any way that suggests the licensor endorses you or your use.

No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

## Files

### Data files

`barela_etal_2023_data1.xlsx`

There are `psychinfo`, `scopus`, and `webofscience` sheets. Each sheet has a different set of metadata sometimes with different names for the same information.

`barela_etal_2023_data2.csv` (details in codebook `barela_etal_2023_codebook.pdf`)

* row - Row number from full data set
* author - Author name(s)
* year - Publication year
* title - Publication title
* journal - Journal name
* volume - Volume number
* issue - Issue number
* page - Page
* doi - Digital object identifier (DOI)
* excluded - Flag indicating reason for exclusion
* exclusion - Flag indicating reason for exclusion
* sample_size - Total sample size
* dog_type - Type of dog included in sample (pet, shelter, working, captive)
* sex - Dog sex ratio (males:females)
* neutered_status - Dog neuter status (neutered:intact)
* tasks - Tasks conducted in study
* dias - Flag indicating whether DIAS was adminstered (0=No, 1=Yes)
* correlate_a-1 - Task and measure for first task in correlation (pair 1)
* correlate_b-1 - Task and measure for second task in correlation (pair 1)
* effectsize_type-1 - Type of effect size for correlation (pair 1)
* effectsize_value-1 - Value of effect size for correlation (pair 1)
* n-1 - Sample size for correlation (pair 1)
* reported_significant-1 - Flag indicating whether correlation was reported as signficant (0=No, 1=Yes) (pair 1)

### R code
 
`barela_etal_2023_rcode.R` - code for running computations and generating figures

### R Markdown documents

`barela_etal_2023.Rmd` - R Markdown document with R code embedded for main manuscript 
`barela_etal_2023_SM.Rmd` - R Markdown document with R code embedded for supplementary materials

### Installation

To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that a subfolder named “figures” is in the folder. Next, open `barela_etal_2023_rcode.R` in [RStudio](https://rstudio.com) or another R interface and ensure that all packages mentioned at the top of the script are installed. Once all packages are installed, run the script in R using `source("barela_etal_2023_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown document `barela_etal_2023.Rmd.` Open this file in RStudio and ensure that you have packages [{knitr}](https://yihui.org/knitr/) and [{rmarkdown}](https://rmarkdown.rstudio.com/) installed. Once installed, use {knitr} to render the document (control-shift-K). Use the same process to render `barela_etal_2023_SM.Rmd`.
