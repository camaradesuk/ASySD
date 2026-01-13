# This function performs additional deduplication with the additional of manually flagged duplicates

This function performs additional deduplication with the additional of
manually flagged duplicates

## Usage

``` r
dedup_citations_add_manual(
  unique_citations,
  merge_citations = TRUE,
  keep_source = NULL,
  keep_label = NULL,
  additional_pairs,
  extra_merge_fields = NULL,
  show_unknown_tags = TRUE
)
```

## Arguments

- unique_citations:

  A dataframe containing citations after automated deduplication

- merge_citations:

  Logical value. Do you want to merge matching citations?

- keep_source:

  Character vector. Selected citation source to preferentially retain in
  the dataset as the unique record

- keep_label:

  Selected citation label to preferentially retain in the dataset as the
  unique record

- additional_pairs:

  dataframe of citations with manual pairs, a subset of the manual pairs
  export. If a `result` column is included, only those with a value of
  `match` will be merged

- extra_merge_fields:

  Add additional fields to merge, output will be similar to the label,
  source, and record_id columns with commas between each merged value

- show_unknown_tags:

  When a label, source, or other merged field is missing, do you want
  this to show as "unknown"?

## Value

Unique citations post manual deduplication

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
res_unique <- result$unique
head(result$manual_dedup)
#> # A tibble: 6 × 41
#>   author1  author2 author title1 title2 title abstract1 abstract2 abstract year1
#>   <chr>    <chr>    <dbl> <chr>  <chr>  <dbl> <chr>     <chr>        <dbl> <chr>
#> 1 Oliveir… de Oli…  0.839 Effec… Effec… 0.888 "OBJECTI… "Objecti…    0.933 2009 
#> 2 Zou T.,… Yan H.…  0.638 Effec… Effec… 0.847 "Introdu… "Introdu…    0.813 2010 
#> 3 Koenig … Abotal…  0.565 Focus… Focus… 0.907 "Skeleta… "Vitamin…    0.775 2010 
#> 4 Davaria… Koenig…  0.594 Focus… Focus… 0.903 "Reducin… "Skeleta…    0.789 2010 
#> 5 Davaria… Abotal…  0.646 Focus… Focus… 0.909 "Reducin… "Vitamin…    0.781 2010 
#> 6 Liu X. … Liu X.…  0.937 Effec… Effec… 0.835 "In a mo… "The eff…    0.769 1997 
#> # ℹ 31 more variables: year2 <chr>, year <dbl>, number1 <chr>, number2 <chr>,
#> #   number <dbl>, pages1 <chr>, pages2 <chr>, pages <dbl>, volume1 <chr>,
#> #   volume2 <chr>, volume <dbl>, journal1 <chr>, journal2 <chr>, journal <dbl>,
#> #   isbn <dbl>, isbn1 <chr>, isbn2 <chr>, doi1 <chr>, doi2 <chr>, doi <dbl>,
#> #   record_id1 <chr>, record_id2 <chr>, label1 <chr>, label2 <chr>,
#> #   source1 <chr>, source2 <chr>, duplicate_id.x <chr>, duplicate_id.y <chr>,
#> #   match <lgl>, min_id <chr>, max_id <chr>

true_dups <- result$manual_dedup[1:5,]
# or equivalently
true_dups <- result$manual_dedup

# You can also use a Shiny interface to review the potential duplicates
# true_dups <- manual_dedup_shiny(result$manual_dedup)

final_result <- dedup_citations_add_manual(res_unique, additional_pairs = true_dups)
#> Joining with `by = join_by(record_id)`
```
