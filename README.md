  <!-- badges: start -->
  [![R-CMD-check](https://github.com/camaradesuk/ASySD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/camaradesuk/ASySD/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
 # Automated Systematic Search Deduplicator (ASySD) <img src="man/figures/updated_logo.png"  width="200px" align="right" />

Removing duplicate references obtained from different databases is an essential step when conducting and updating systematic literature reviews. ASySD (pronounced "assist") is a tool to automatically identify and remove duplicate records. 

## Shiny application
This tool is available both as  an R package and a user-friendly [shiny application](https://camarades.shinyapps.io/ASySD/). 

## Tutorial
Please check out the new [ASySD tutorial on youtube](https://www.youtube.com/watch?v=oCR1A_heFcs&t=42s). We would like to extend a huge thank you to ESMARConf for giving us a platform to showcase ASySD within the evidence synthesis community!

## Tool performance
An evaluation of ASySD's performance versus other automated deduplication tools is available [as a preprint](https://www.biorxiv.org/content/10.1101/2021.05.04.442412v1)

## Installation:

```{r}
install.packages("devtools")
devtools::install_github("camaradesuk/ASySD")
```
## Automatically deduplicate citation data

```{r}
# load citations 
citation_data <- load_search(filepath, method="endnote")

# deduplicate
dedup_citations <- dedup_citations(citation_data)

# get unique citation dataframe
unique_citations <- dedup_citations$unique 

```
