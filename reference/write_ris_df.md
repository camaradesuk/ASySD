# Export a Data Frame to RIS Format

`write_ris_df()` exports a data frame containing bibliographic
information to a `.ris` file suitable for EndNote, Zotero, or other
reference managers. This function properly handles multiple authors,
keywords, URLs, ISBNs, and accession numbers. Missing values are
automatically converted to empty strings.

## Usage

``` r
write_ris_df(df, file)
```

## Arguments

- df:

  A data frame containing bibliographic information. Standard columns
  include:

  - `author` — Authors separated by `;` (e.g., `"Smith, J.; Doe, A."`)

  - `title` — Article or book title

  - `year` — Publication year

  - `journal` — Journal name

  - `volume` — Volume

  - `number` — Issue number

  - `pages` — Page range

  - `doi` — DOI string

  - `abstract` — Abstract text

  - `keywords` — Keywords separated by `;`

  - `url` — URL or link to article

  - `isbn` — ISBN number

  - `accession_number` — Accession number

  - `type` — Reference type (e.g., `"Journal Article"`)

  - `label` — Optional label

  - `source` — Database source

  - `database` — Database name

- file:

  Character. Path to the `.ris` file to write.

## Value

Invisibly returns the RIS character vector that was written to file. The
primary effect is writing the RIS file.

## Details

The function ensures that all `NA` values in the data frame are
converted to empty strings. Multiple authors and keywords are separated
using the specified separators. The resulting RIS file can be imported
into EndNote, Zotero, Mendeley, or other reference managers that support
RIS.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  author = c("Smith, J.; Doe, A.", "Brown, B."),
  title = c("Example Paper 1", "Example Paper 2"),
  year = c(2020, 2021),
  journal = c("Journal A", "Journal B"),
  type = c("Journal Article", "Journal Article"),
  keywords = c("AI; LLM", "Machine Learning; NLP"),
  doi = c("10.1234/example1", "10.1234/example2"),
  stringsAsFactors = FALSE
)
write_ris_df(df, file = "example.ris")
} # }
```
