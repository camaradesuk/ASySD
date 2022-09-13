# Automated Systematic Search Deduplicator (ASySD)
![image](https://user-images.githubusercontent.com/46422034/190010609-fb2695ed-8fc6-45a6-a430-516bc952c9bf.png)


Removing duplicate references obtained from different databases is an essential step when conducting and updating systematic literature reviews. ASySD (pronounced "assist") is a tool to automatically identify and remove duplicate records. This tool is available as an R package and a user-friendly [shiny application](https://camarades.shinyapps.io/RDedup/). 

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
