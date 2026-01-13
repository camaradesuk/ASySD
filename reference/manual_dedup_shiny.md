# A Shiny interface to review potential duplicates

[`dedup_citations()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations.md)
can return potential duplicates for manual review. This function takes
these potential duplicates and provides a Shiny interface to review them
and select those that should be deduplicated. The output can then be
passed to
[`dedup_citations_add_manual()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations_add_manual.md)
to complete the deduplication, or be used to call this function again if
manual review is not yet complete.

## Usage

``` r
manual_dedup_shiny(df, cols = names(df))
```

## Arguments

- df:

  A dataframe containing potential duplicate entries, typically returned
  by
  [`dedup_citations()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations.md).

- cols:

  A character vector of column names to display during the review
  process. By default, uses all columns in `df`.

## Value

The dataframe with an updated `result` column, indicating whether each
entry is a duplicate (`"match"`) or not (`"no_match"`). This can be
passed to
[`dedup_citations_add_manual()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations_add_manual.md)
for completing the deduplication process.

The dataframe with a `result` column indicating whether the entry
constitutes a duplicate - to be passed to
[`dedup_citations_add_manual()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations_add_manual.md)

## Examples

``` r
if (FALSE) { # interactive()

# Perform deduplication
result <- dedup_citations(citations_df, keep_source="Embase")

# Manually review potential duplicates
manual_review <- manual_dedup_shiny(result$manual_dedup)

# Complete deduplication
final_result <- dedup_citations_add_manual(result$unique, additional_pairs = manual_review)
}
```
