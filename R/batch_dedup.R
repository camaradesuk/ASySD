#' Batch deduplication for large searches
#'
#' This function splits citations into batches, deduplicates each batch, and binds results together into one dataframe of unique citations.
#' @details The following fields will be used in `citations` (if provided): record_id, author, year, journal, doi, title, pages, volume, number, abstract, isbn, label, source
#' @param citations A dataframe containing citation information.
#' @param batch_n Numeric value specifying the maximum number of citations per batch. Default is 50000.
#' @param keep_source Character vector specifying the citation source(s) to preferentially retain in the dataset as the unique record.
#' @param keep_label Character vector specifying the citation label(s) to preferentially retain in the dataset as the unique record.
#' @param sort_by Character vector specifying the sorting criteria. Default is c("year", "title","author"). Valid options are column names of the citations dataframe.
#' @return A list with components:
#' * unique - dataframe containing unique citations.
#' * manual_dedup - dataframe containing citations to be manually checked for duplicates.
#' @import dplyr
#' @export
#' @examples
#'
#' # Perform batch deduplication
#' result <- batch_dedup(citations_df, batch_n = 250)
#'
#' # View unique citations
#' head(result$unique)
#'
#'
batch_dedup <- function(citations, batch_n = 50000, keep_source=NULL, keep_label=NULL, sort_by = c("year", "title","author")){

  message("Splitting up dataframe")

  unique <- list()
  manual <- list()

  # Convert sortby to lowercase to match column names
  sort_by <- tolower(sort_by)

  # Check if sort by values are valid column names
  valid_sortby <- sort_by[sort_by %in% names(citations)]

  if (length(valid_sortby) == 0) {
    stop("Invalid sort_by criteria. Please provide valid column names.")
  }

  # Arrange dataframe based on sortby criteria
  citations <- citations %>%
    arrange_at(valid_sortby)


  split_citations <- split(citations, ceiling(seq(nrow(citations))/batch_n))

  for(i in 1:length(split_citations)){

    suppressWarnings({
      suppressMessages({
        # Perform deduplication for the current year's citations
        dedup_results <- dedup_citations(split_citations[[i]], merge_citations = TRUE,
                                         keep_source = keep_source, keep_label = keep_label)
      })
    })

    # Append deduplicated results to the list
    unique[[i]] <- dedup_results$unique
    manual[[i]] <- dedup_results$manual
    message(paste0("batch ", i, " complete \U2714"))
  }

  res_unique <- bind_rows(unique)
  res_manual <- bind_rows(manual)


  if ("record_id" %in% colnames(res_unique)){

    res_unique <- res_unique %>%
      select(-record_id)

  }

  if (!("record_ids" %in% colnames(res_unique))){

    warning("No duplicates detected... returning original dataframe")

    return(list("unique" = res_unique))

  }

  message(paste0("identified ", nrow(res_unique), " unique citations"))

  return(list("unique" = res_unique,
              "manual_dedup" = res_manual))

}
