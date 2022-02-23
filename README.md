# Automated Systematic Search Deduplicator (ASySD)

Removing duplicate references obtained from different databases is an essential step when conducting and updating systematic literature reviews. ASySD is a tool to automatically identify and remove duplicate records.  

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
