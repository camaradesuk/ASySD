<!-- badges: start --> [![R-CMD-check](https://github.com/camaradesuk/ASySD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/camaradesuk/ASySD/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

\# Automated Systematic Search Deduplicator (ASySD) <img src="man/figures/updated_logo.png" width="200px" align="right"/>

Removing duplicate references obtained from different databases is an essential step when conducting and updating systematic literature reviews. ASySD (pronounced "assist") is a tool to automatically identify and remove duplicate records.

## Shiny application

This tool is available both as an R package and a user-friendly [shiny application](https://camarades.shinyapps.io/ASySD/).

## Tutorial

Please check out the new [ASySD tutorial on youtube](https://www.youtube.com/watch?v=oCR1A_heFcs&t=42s). We would like to extend a huge thank you to ESMARConf for giving us a platform to showcase ASySD within the evidence synthesis community!

## Tool performance

An evaluation of ASySD's performance versus other automated deduplication tools is available [as a preprint](https://www.biorxiv.org/content/10.1101/2021.05.04.442412v1)

## Installation:

```{r}
install.packages("devtools")
devtools::install_github("camaradesuk/ASySD")
```

## Requirements

If loading with the ASySD package functions, the required fields will be automatically created based on the input data. If metadata columns are missing, they will be automatically created by ASySD and set to NA. For optimal performance, we recommend having as much metadata as possible.

If using a dataframe, you should ideally have the following column names:

| **Name**              | **Definition**                                                                                                                                                                        |
|---------------|--------------------------------------------------------|
| **author**            | The author(s) of the publication                                                                                                                                                      |
| **year**              | The year the publication was published                                                                                                                                                |
| **journal**           | The name of the journal in which the publication appeared                                                                                                                             |
| **doi**               | The Digital Object Identifier (DOI) assigned to the publication                                                                                                                       |
| **title**             | The title of the publication                                                                                                                                                          |
| **pages**             | The page numbers of the publication                                                                                                                                                   |
| **volume**            | The volume number of the publication (if applicable)                                                                                                                                  |
| **number**            | The issue number of the publication (if applicable)                                                                                                                                   |
| **abstract**          | Abstract of publication                                                                                                                                                               |
| **record_id**         | A unique identifier for the publication. If this is not obtained from the citation file, ASySD will genereate an id for each citation based on row numbers.                           |
| **isbn**              | The International Standard Book Number (ISBN) assigned to the publication (if applicable). If unavailable, the International Standard Serial Number can be used here instead (ISSN).  |
| **label (optional)**  | A label or tag assigned to the publication (if applicable) - for example, **new search** or **old search**                                                                            |
| **source (optional)** | The source or database from which the publication was obtained - for example **wos**, **embase**, **pubmed**, **scopus**                                                              |

## Automatically deduplicate citation data

```{r}
# load citations 
citation_data <- load_search(filepath, method="endnote")

# deduplicate
dedup_citations <- dedup_citations(citation_data)

# get unique citation dataframe
unique_citations <- dedup_citations$unique 

```
