# Deduplicating large datasets

## **Setup**

First, install and load the ASySD package.

``` r
# devtools::install_github("camaradesuk/ASySD")
library(ASySD)
```

## Loading data

First, load citations from an Endnote XML file using the
[`load_search()`](https://camaradesuk.github.io/ASySD/reference/load_search.md)
function. Alternatively, you can upload other file types such as .csv
files by changing the method argument.

``` r
citations <- load_search("systematic_search.xml", method="endnote")
```

## Batch deduplication

To handle large datasets effectively, we recommend running deduplication
in batches. This approach is especially useful for datasets with over
100,000 records. Set the batch_n parameter to control batch size, with a
default value of 50,000. Here, we illustrate batching on a smaller scale
for demonstration purposes.

``` r
results <- batch_dedup(citations, batch_n=2000, sort_by = c("year", "title","author"))
#> Splitting up dataframe
#> batch 1 complete ✔
#> batch 2 complete ✔
#> batch 3 complete ✔
#> batch 4 complete ✔
#> batch 5 complete ✔
#> identified 5457 unique citations
```

## Additional rounds of duplication

After the initial deduplication, further refinement may be necessary.
Duplicates may have been separated into different batches in the above
example - such as if the years differ substantially or if one year is
missing.

You can perform additional rounds of deduplication using different
sorting criteria, such as the title alone. Using the results from the
first round of deduplication as input, you can run the batch
deduplication again and check the results. In this instance, running
again identified 3 additional duplicates.

``` r
# get unique results from round 1
unique_r1 <- results$unique

# deduplicate again using unique results, setting different sort criteria
results_r2 <- batch_dedup(unique_r1, batch_n=2000, sort_by = c("title"))
#> Splitting up dataframe
#> batch 1 complete ✔
#> batch 2 complete ✔
#> batch 3 complete ✔
#> identified 5454 unique citations

# get results after 2 rounds of deduplication
unique_r2 <- results_r2$unique
```

## Exporting results

Once deduplication is complete, you can export the unique records to a
file for import into reference managers or systematic review software.

``` r
write_citations(unique_r2, type="txt", filename="unique.txt")
```
