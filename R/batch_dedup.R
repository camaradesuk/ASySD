####------ Deduplicate searches in batches ------ ####

#' This function processes a dataframe of citations, deduplicates them, and provides unique citation results. If the number of citations exceeds a specified threshold, it splits the dataframe and performs deduplication on each batch.
#'
#' @param citations A dataframe containing citation information.
#' @param batch_n Numeric value specifying the maximum number of citations per batch. Default is 50000.
#' @param keep_source Character vector specifying the citation source(s) to preferentially retain in the dataset as the unique record.
#' @param keep_label Character vector specifying the citation label(s) to preferentially retain in the dataset as the unique record.
#' @param sort_by Character vector specifying the sorting criteria. Default is c("year", "title","author"). Valid options are column names of the citations dataframe.
#' @return A list with components:
#'   \describe{
#'     \item{unique}{Dataframe containing unique citations.}
#'     \item{manual_dedup}{Dataframe containing citations to be manually checked for duplicates.}
#'   }
#' @import dplyr
#' @export

batch_dedup <- function(citations, batch_n = 50000, keep_source=NULL, keep_label=NULL, sort_by = c("year", "title","author")){

  message("Splitting up dataframe")

  unique <- list()
  manual <- list()

  # Convert sortby to lowercase to match column names
  sort_by <- tolower(sort_by)

  # Check if sortby values are valid column names
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
