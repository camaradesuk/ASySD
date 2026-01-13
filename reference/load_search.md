# Load in citations for deduplication

This function loads a citations file using the specified import method.
If the method is not provided, it defaults to one based on the file
extension. Supported methods: "endnote", "csv", "txt", "bib",
"zotero_csv", "ris".

## Usage

``` r
load_search(path, method = NULL)
```

## Arguments

- path:

  File path(s) to one or more citations file(s). If more than one file
  is passed, the file name is added into the `source` field to
  distinguish where citations came from

- method:

  Import method. Valid options are "endnote", "csv", "txt", "bib",
  "zotero_csv", and "ris". If not provided, the method will be inferred
  from the file extension.

## Value

A dataframe of the citations.
