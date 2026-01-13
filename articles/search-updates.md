# Updating a Systematic Search

## **Setup**

First, install and load the ASySD package.

``` r
# devtools::install_github("camaradesuk/ASySD")library(ASySD)
library(ASySD)
```

## **Loading citation data**

Load citations from an **existing search** file using the
[`load_search()`](https://camaradesuk.github.io/ASySD/reference/load_search.md)
function. In this example, we use a csv format.

``` r
existing_search <- load_search("old_sr_search.csv")
```

Load citations from a **new systematic search.**

``` r
new_search <- load_search("new_sr_search.csv")
```

### Combine old and new citation data

Before deduplication, we must bind the citations into one dataframe.
First, give each search a different source so that we can specify which
citations to retain.

``` r
existing_search$source <- "old"
new_search$source <- "new"

all_citations <- plyr::rbind.fill(existing_search, new_search)
```

## Automated deduplication

Remove duplicate citations automatically using the `dedup_citations`
function. Here we have specified the argument `merge=TRUE` to indicate
that we want to merge duplicate records and have a record of which
citations have been merged into one. We have specified in the
`keep_source` argument that we wish to preferentially retain old
citations. In practice, this means that the duplicate_id chosen for a
set of records will preferentially be the record_id of a citation in the
OLD systematic search. This is to facilitate easy record linkage - see
later.

``` r
results <- dedup_citations(all_citations, merge_citations = TRUE, keep_source = "old")
#> formatting data...
#> identifying potential duplicates...
#> identified duplicates!
#> flagging potential pairs for manual dedup...
#> Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#> 8972 citations loaded...
#> 472 duplicate citations removed...
#> 8500 unique citations remaining!
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

To check for additional duplicates, get the dataframe of citations for
manual review. You can review within R or export as a csv / excel file
to go through each row of pairs.

``` r
potential_duplicates <- results$manual_dedup
```

After reviewing the pairs, create a dataframe contianing **only the true
duplicate pairs.** Here, all the suggested duplicates look like REAL
duplicates. Alternatively, you could go through them one-by-one using
the
[`manual_dedup_shiny()`](https://camaradesuk.github.io/ASySD/reference/manual_dedup_shiny.md)
function.

``` r
true_duplicates <- potential_duplicates
```

Now, to get the final deduplication results, use the
[`dedup_citations_add_manual()`](https://camaradesuk.github.io/ASySD/reference/dedup_citations_add_manual.md)function.
To account for additional duplicates you have reviewed, add them into
the *additional_pairs* argument.

``` r
final_results <- dedup_citations_add_manual(unique_citations, additional_pairs = true_duplicates, merge_citations = TRUE, keep_source = "old")
#> Joining with `by = join_by(record_id)`
```

## Find new citations identified in update

Now we have a final set of unique citations, how can we find the new
citations we added with our latest systematic search?

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
new_citations <- final_results %>%
  filter(source == "new") 

new_citations %>%
   tail(3) %>%
   gt::gt() %>%
   gt::cols_hide(c(abstract))
```

| duplicate_id        | author                                                                                                                                                                                               | year | journal                                        | doi                                 | title                                                                                                                       | pages   | volume | number | isbn | label  | source | url | ...1 | uid                 | keywords                                                                                             | secondarytitle | issn      | pmid | ptype | author_country | author_affiliation | record_ids          |
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|------------------------------------------------|-------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|---------|--------|--------|------|--------|--------|-----|------|---------------------|------------------------------------------------------------------------------------------------------|----------------|-----------|------|-------|----------------|--------------------|---------------------|
| wos:000931052000002 | Lipton, Stuart A.                                                                                                                                                                                    | 2022 | FREE RADICAL BIOLOGY AND MEDICINE              | 10.1016/j.freeradbiomed.2022.10.272 | Hidden networks of aberrant protein transnitrosylation contribute to synapse loss in Alzheimer's disease                    | 171-176 | 193    | NA     | NA   | 270223 | new    | NA  | 5561 | wos:000931052000002 | NA                                                                                                   | NA             | 0891-5849 | NA   | NA    | NA             | NA                 | wos:000931052000002 |
| wos:000931426100001 | Marini, Sandro; Chung, Jaeyoon; Han, Xudong; Sun, Xinyu; Parodi, Livia; Farrer, Lindsay A.; Rosand, Jonathan; Romero, Jose Rafael; Anderson, Christopher D.                                          | 2023 | INTERNATIONAL JOURNAL OF STROKE                | 10.1177/17474930231155816           | Pleiotropy analysis between lobar intracerebral hemorrhage and CSF beta-amyloid highlights new and established associations | NA      | NA     | NA     | NA   | 270223 | new    | NA  | 9861 | wos:000931426100001 | ICH \| beta-amyloid \| pleiotropy \| genetic epidemiology \| cadherin \| cerebral amyloid angiopathy | NA             | 1747-4930 | NA   | NA    | NA             | NA                 | wos:000931426100001 |
| wos:000932018500001 | Walker, Keenan A.; Duggan, Michael R.; Gong, Zhaoyuan; Dark, Heather E.; Laporte, John P.; Faulkner, Mary E.; An, Yang; Lewis, Alexandria; Moghekar, Abhay R.; Resnick, Susan M.; Bouhrara, Mustapha | 2023 | ANNALS OF CLINICAL AND TRANSLATIONAL NEUROLOGY | 10.1002/acn3.51730                  | Kidney and lung crosstalk during critical illness: large-scale cohort study (FEB, 10.1007/s40620-023-01594-z, 2023)         | NA      | NA     | NA     | NA   | 270223 | new    | NA  | 6501 | wos:000932018500001 | NA                                                                                                   | NA             | 2328-9503 | NA   | NA    | NA             | NA                 | wos:000932018500001 |

Lets also have a look at the citations identified in both searches by
removing citations with a single source.

``` r
crossover <- final_results %>%
  filter(!source == "new") %>%
  filter(!source == "old") 

crossover %>%
   tail(3) %>%
   gt::gt() %>%
   gt::cols_hide(c(abstract))
```

| duplicate_id        | author                                                                                                                                                                  | year | journal                                          | doi                          | title                                                                                                                                                                                    | pages | volume | number | isbn | label          | source   | url | ...1 | uid                 | keywords                                                                                                                                        | secondarytitle | issn      | pmid     | ptype   | author_country | author_affiliation | record_ids                                     |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|--------------------------------------------------|------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|--------|--------|------|----------------|----------|-----|------|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|----------------|-----------|----------|---------|----------------|--------------------|------------------------------------------------|
| wos:000919545100001 | Larkin, Howard D. D.                                                                                                                                                    | 2023 | JAMA-JOURNAL OF THE AMERICAN MEDICAL ASSOCIATION | 10.1001/jama.2022.24490      | Lecanemab Gains FDA Approval for Early Alzheimer Disease                                                                                                                                 | 363   | 329    | NA     | NA   | 270223, 270223 | new, new | NA  | 3271 | wos:000919545100001 | NA                                                                                                                                              | NA             | 0098-7484 | 36652625 | Article | NA             | NA                 | wos:000919545100001, scopus-2-s2.0-85147720543 |
| wos:000924510300006 | Chen, Shanquan; Price, Annabel C.; Cardinal, Rudolf N.; Moylett, Sinead; Kershenbaum, Anne D.; Fitzgerald, James; Mueller, Christoph; Stewart, Robert; O'Brien, John T. | 2022 | PLOS MEDICINE                                    | 10.1371/journal.pmed.1004124 | Association between antidementia medication use and mortality in people diagnosed with dementia with Lewy bodies in the UK: A retrospective cohort study                                 | NA    | 19     | NA     | NA   | 270223, 270223 | new, new | NA  | 9981 | wos:000924510300006 | NA                                                                                                                                              | NA             | 1549-1277 | NA       | NA      | NA             | NA                 | wos:000924510300006, wos:000925010400002       |
| wos:000928044600001 | Wang, Lin-Yu; Liu, Jiao; Peng, Yi-Zhu; Zhang, Cai-Ping; Zou, Wei; Liu, Feng; Zhan, Ke-Bin; Zhang, Ping                                                                  | 2022 | NATURAL PRODUCT COMMUNICATIONS                   | 10.1177/1934578X221141162    | Curcumin-Nicotinate Attenuates Hippocampal Synaptogenesis Dysfunction in Hyperlipidemia Rats by the BDNF/TrkB/CREB Pathway: Involving Idol/LDLR Signaling to Eliminate A beta Deposition | NA    | 17     | NA     | NA   | 270223, 270223 | new, new | NA  | 9791 | wos:000928044600001 | hyperlipidemia \| high-fat diet \| Curcumin-Nicotinate \| amyloid-beta \| BDNF \| TrkB \| CREB signaling \| synaptogenesis \| Idol/LDLR pathway | NA             | 1934-578X | NA       | NA      | NA             | NA                 | wos:000928044600001, wos:000922862000001       |

To keep good records, we don’t want to lose track of identifiers for
studies we have already included in a review. This is why specifying the
citation to keep was important! To illustrate this, look specifically at
the citations present in the old search.

``` r
old_citations <- final_results %>%
  filter(grepl("old", source)) # find all citations in old search
```

We can check that the duplicate ids here refer to the original record id
in the existing_citations dataframe we imported. As you can see, they
are all present. In the record_ids column you can see the different
record_ids that have merged into a single citation. In case you make a
mistake or don’t specify the record_id to keep as the duplicate_id, you
can use these to trace back your citations to the original dataframes.

``` r
old_citations_check <- old_citations %>%
  filter(duplicate_id %in% existing_search$record_id) #check that all citations use the OLD record_id as the duplicate_id

crossover %>%
   tail(3) %>%
   gt::gt() %>%
   gt::cols_hide(c(abstract))
```

| duplicate_id        | author                                                                                                                                                                  | year | journal                                          | doi                          | title                                                                                                                                                                                    | pages | volume | number | isbn | label          | source   | url | ...1 | uid                 | keywords                                                                                                                                        | secondarytitle | issn      | pmid     | ptype   | author_country | author_affiliation | record_ids                                     |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|--------------------------------------------------|------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|--------|--------|------|----------------|----------|-----|------|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|----------------|-----------|----------|---------|----------------|--------------------|------------------------------------------------|
| wos:000919545100001 | Larkin, Howard D. D.                                                                                                                                                    | 2023 | JAMA-JOURNAL OF THE AMERICAN MEDICAL ASSOCIATION | 10.1001/jama.2022.24490      | Lecanemab Gains FDA Approval for Early Alzheimer Disease                                                                                                                                 | 363   | 329    | NA     | NA   | 270223, 270223 | new, new | NA  | 3271 | wos:000919545100001 | NA                                                                                                                                              | NA             | 0098-7484 | 36652625 | Article | NA             | NA                 | wos:000919545100001, scopus-2-s2.0-85147720543 |
| wos:000924510300006 | Chen, Shanquan; Price, Annabel C.; Cardinal, Rudolf N.; Moylett, Sinead; Kershenbaum, Anne D.; Fitzgerald, James; Mueller, Christoph; Stewart, Robert; O'Brien, John T. | 2022 | PLOS MEDICINE                                    | 10.1371/journal.pmed.1004124 | Association between antidementia medication use and mortality in people diagnosed with dementia with Lewy bodies in the UK: A retrospective cohort study                                 | NA    | 19     | NA     | NA   | 270223, 270223 | new, new | NA  | 9981 | wos:000924510300006 | NA                                                                                                                                              | NA             | 1549-1277 | NA       | NA      | NA             | NA                 | wos:000924510300006, wos:000925010400002       |
| wos:000928044600001 | Wang, Lin-Yu; Liu, Jiao; Peng, Yi-Zhu; Zhang, Cai-Ping; Zou, Wei; Liu, Feng; Zhan, Ke-Bin; Zhang, Ping                                                                  | 2022 | NATURAL PRODUCT COMMUNICATIONS                   | 10.1177/1934578X221141162    | Curcumin-Nicotinate Attenuates Hippocampal Synaptogenesis Dysfunction in Hyperlipidemia Rats by the BDNF/TrkB/CREB Pathway: Involving Idol/LDLR Signaling to Eliminate A beta Deposition | NA    | 17     | NA     | NA   | 270223, 270223 | new, new | NA  | 9791 | wos:000928044600001 | hyperlipidemia \| high-fat diet \| Curcumin-Nicotinate \| amyloid-beta \| BDNF \| TrkB \| CREB signaling \| synaptogenesis \| Idol/LDLR pathway | NA             | 1934-578X | NA       | NA      | NA             | NA                 | wos:000928044600001, wos:000922862000001       |

## Exporting results

Once deduplication is complete, you can export the new unique records to
a file for import into reference managers or systematic review software.

``` r
write_citations(new_citations, type="txt", filename="unique.txt")
```
