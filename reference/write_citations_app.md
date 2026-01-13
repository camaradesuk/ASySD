# This function writes citation data to disk in different formats

This function writes citation data to disk in different formats

## Usage

``` r
write_citations_app(citations, type = c("ris", "txt", "csv", "bib"), filename)
```

## Arguments

- citations:

  A dataframe containing citations - usually post-deduplication

- type:

  export type

- filename:

  output file name

## Value

file export

## Examples

``` r
# Create sample citations dataframe
citations <- data.frame(
  author = c("Author1", "Author2"),
  year = c(2000, 2001),
  title = c("Title 1", "Title 2"),
  journal = c("Journal A", "Journal B"),
  doi = c("doi1", "doi2"),
  pages = c("1-10", "20-30"),
  volume = c("Vol 1", "Vol 2"),
  number = c("Issue 1", "Issue 2"),
  abstract = c("Abstract 1", "Abstract 2"),
  isbn = c("123456789", "987654321"),
  file_name = c("", ""),
  record_id = c(1, 2),
  duplicate_id = c(1, 2),
  label = c("Label A", "Label B"),
  source = c("Source A", "Source B"),
  stringsAsFactors = FALSE
)

# Example usage for exporting to txt format
write_citations_app(citations, type = "txt", filename = "citations.txt")
unlink("citations.txt")
```
