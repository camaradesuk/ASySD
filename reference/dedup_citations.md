# This function deduplicates citation data

This function deduplicates citation data

## Usage

``` r
dedup_citations(
  raw_citations,
  manual_dedup = TRUE,
  merge_citations = TRUE,
  keep_source = NULL,
  keep_label = NULL,
  extra_merge_fields = NULL,
  show_unknown_tags = TRUE,
  user_input = NA
)
```

## Arguments

- raw_citations:

  A dataframe containing duplicate ciations

- manual_dedup:

  Logical value. Do you want to retrieve dataframe for manual
  deduplication?

- merge_citations:

  Logical value. Do you want to merge matching citations?

- keep_source:

  Character vector. Selected citation source to preferentially retain in
  the dataset as the unique record

- keep_label:

  Selected citation label to preferentially retain in the dataset as the
  unique record

- extra_merge_fields:

  Add additional fields to merge, output will be similar to the label,
  source, and record_id columns with commas between each merged value

- show_unknown_tags:

  When a label, source, or other merged field is missing, do you want
  this to show as "unknown"?

- user_input:

  Do you want to proceed if important columns are missing? 1-yes; 2-no

## Value

A list of 2 dataframes - unique citations and citations to be manually
deduplicated if option selected

## Details

The following fields will be used in `raw_citations` (if provided):
record_id, author, year, journal, doi, title, pages, volume, number,
abstract, isbn, label, source

## Examples

``` r
# Perform deduplication
result <- dedup_citations(citations_df, keep_source="Embase")
#> formatting data...
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#> 1001 citations loaded...
#> 392 duplicate citations removed...
#> 609 unique citations remaining!

# View unique citations
head(result$unique)
#> # A tibble: 6 × 16
#>   duplicate_id author     year  journal doi   title pages volume number abstract
#>   <chr>        <chr>      <chr> <chr>   <chr> <chr> <chr> <chr>  <chr>  <chr>   
#> 1 1018         Della-Mor… 2012  Pharma… 10.2… Gene… 1741… 13     15     A subth…
#> 2 1107         Downey J.… 2008  Expert… 10.1… Free… 589-… 6      5      NA      
#> 3 1127         Duan D. Y… 2005  Acta P… 10.1… Func… 265-… 26     3      In comp…
#> 4 1184         Eskildsen… 1996  Ann N … 10.1… Expl… 210-… 793    NA     NA      
#> 5 1288         Fryer R. … 2001  Am J P… 10.1… Esse… H134… 280    3      Stimula…
#> 6 1290         Fryer R. … 2001  Basic … 10.1… ERK … 136-… 96     2      Opioids…
#> # ℹ 6 more variables: isbn <chr>, secondary_title <chr>, label <chr>,
#> #   url <chr>, source <chr>, record_ids <chr>

```
