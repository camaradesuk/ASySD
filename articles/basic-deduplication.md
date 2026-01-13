# Simple Deduplication

## **Setup**

First, install and load the ASySD package.

``` r
# devtools::install_github("camaradesuk/ASySD")
library(ASySD)
```

## **Loading Citation Data**

Begin by loading your citation data from an Endnote XML file using the
**[`load_search()`](https://camaradesuk.github.io/ASySD/reference/load_search.md)**
function. You can specify alternative file formats such as CSV, RIS, or
BIB if needed.

``` r
citations <- load_search("systematic_search.xml", method="endnote")
```

## Automated deduplication

Remove duplicate citations automatically using the `dedup_citations`
function.

``` r
results <- dedup_citations(citations, merge_citations = TRUE)
#> formatting data...
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#> 8948 citations loaded...
#> 3508 duplicate citations removed...
#> 5440 unique citations remaining!
```

The `dedup_citations` function returns a list of two dataframes by
default. The first contains unique citations after duplicates were
removed automatically by ASySD. In most cases, this will remove the vast
majority of duplicates. There will likely be some duplicates remaining
which need manual review by a human (see next step).

``` r
unique_citations <- results$unique
```

## Manual deduplication

Review the pairs using the
[`manual_dedup_shiny()`](https://camaradesuk.github.io/ASySD/reference/manual_dedup_shiny.md)
function. This opens up and interactive shiny app to allow you to go
through each potential duplicate pair and determine whether it
represents a true duplicate (press 1) or not (press 3). Save the output
to an object so you can use it in the next step.

True duplicates will have a “match” in the result column. Non duplicates
with have a “no match” in the result column.

``` r
post_manual_review <- manual_dedup_shiny(potential_duplicates) 
```

## **Final Deduplication**

Combine the results of automated and manual deduplication using the
[`dedup_citations_add_manual()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations_add_manual.md)
function. Include the results of manual deduplication using the
`additional_pairs` argument. The rows where the result column = “match”
will be considered by ASySD to be additional duplicates.

``` r
final_results <- dedup_citations_add_manual(unique_citations,additional_pairs = post_manual_review)
```

## **Exporting Results**

You can now write your results to a file for import into a reference
manager or systematic review software

``` r
write_citations(final_results, type="txt", filename="citations.txt")
```
