# This function generates a duplicate ID for sets of matching citations

This function generates a duplicate ID for sets of matching citations

## Usage

``` r
merge_metadata(matched_pairs_with_ids, extra_merge_fields)
```

## Arguments

- matched_pairs_with_ids:

  citation data with duplicate ids

- extra_merge_fields:

  Add additional fields to merge, output will be similar to the label,
  source, and record_id columns with commas between each merged value

## Value

Dataframe of formatted citation data with duplicate id
