# Batch deduplication for large searches

This function splits citations into batches, deduplicates each batch,
and binds results together into one dataframe of unique citations.

## Usage

``` r
batch_dedup(
  citations,
  batch_n = 50000,
  keep_source = NULL,
  keep_label = NULL,
  sort_by = c("year", "title", "author")
)
```

## Arguments

- citations:

  A dataframe containing citation information.

- batch_n:

  Numeric value specifying the maximum number of citations per batch.
  Default is 50000.

- keep_source:

  Character vector specifying the citation source(s) to preferentially
  retain in the dataset as the unique record.

- keep_label:

  Character vector specifying the citation label(s) to preferentially
  retain in the dataset as the unique record.

- sort_by:

  Character vector specifying the sorting criteria. Default is c("year",
  "title","author"). Valid options are column names of the citations
  dataframe.

## Value

A list with components:

- unique - dataframe containing unique citations.

- manual_dedup - dataframe containing citations to be manually checked
  for duplicates.

## Details

The following fields will be used in `citations` (if provided):
record_id, author, year, journal, doi, title, pages, volume, number,
abstract, isbn, label, source

## Examples

``` r
# Perform batch deduplication
result <- batch_dedup(citations_df, batch_n = 250)
#> Splitting up dataframe
#> batch 1 complete ✔
#> batch 2 complete ✔
#> batch 3 complete ✔
#> batch 4 complete ✔
#> batch 5 complete ✔
#> identified 611 unique citations

# View unique citations
head(result$unique)
#> # A tibble: 6 × 16
#>   duplicate_id author     year  journal doi   title pages volume number abstract
#>   <chr>        <chr>      <chr> <chr>   <chr> <chr> <chr> <chr>  <chr>  <chr>   
#> 1 1075         Doenst T.… 1996  Am J P… 10.1… Fast… H160… 270    5 Pt 2 We test…
#> 2 1092         Dorheim T… 1991  Surgery NA    Enha… 136-… 110    2      Reversi…
#> 3 1182         Erikson J… 1996  Am Hea… 10.1… Endo… 84-90 132    1 Pt 1 This st…
#> 4 1184         Eskildsen… 1996  Ann N … 10.1… Expl… 210-… 793    NA     NA      
#> 5 1210         Faris B.,… 1997  Ann Th… 10.1… Fail… 1735… 64     6      BACKGRO…
#> 6 1211         Fatehi-Ha… 1997  Eur J … 10.1… Geni… 67-70 338    1      The pos…
#> # ℹ 6 more variables: isbn <chr>, secondary_title <chr>, label <chr>,
#> #   url <chr>, source <chr>, record_ids <chr>

```
