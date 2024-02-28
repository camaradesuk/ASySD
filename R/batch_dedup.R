####------ Deduplicate large searches ------ ####
#' Get new unique citations and perform deduplication
#'
#' This function processes a dataframe of citations, deduplicates them, and provides unique citation results.
#'
#' @param citations A dataframe containing citation information.
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#'  @import dplyr
#' @return A list with components:
#'   \describe{
#'     \item{unique}{Dataframe containing unique citations.}
#'     \item{manual_dedup}{Dataframe containing citations to be manually checked for duplicates.}
#'   }
#' @export

dedup_large_search <- function(citations, keep_source=NULL, keep_label=NULL){

  if(length(citations$record_id) > 50000){

    message("Splitting up dataframe and running multiple deduplications due to size...")

    unique <- list()
    manual <- list()

    citations <- citations %>%
      arrange(year, title, author)

    split_citations <- split(citations, ceiling(seq(nrow(citations))/50000))

    for(i in 1:length(split_citations)){

      # Perform deduplication for the current year's citations
      dedup_results <- ASySD::dedup_citations(split_citations[[i]], merge_citations = TRUE,
                                              keep_source = keep_source, keep_label = keep_label)

      # Append deduplicated results to the list
      unique[[i]] <- dedup_results$unique
      manual[[i]] <- dedup_results$manual
    }

    res_unique <- bind_rows(unique)
    res_manual <- bind_rows(manual)

  } else {

    dedup_results <- ASySD::dedup_citations(citations, merge_citations = TRUE, keep_source = keep_source)

    res_unique <- dedup_results$unique
    res_manual <- dedup_results$manual

  }

  if (!("record_ids" %in% colnames(res_unique))){

    warning("No duplicates detected... returning original dataframe")

    return(list("unique" = res_unique))

  }

  message(paste0("identified ", nrow(res_unique), " unique citations"))

  return(list("unique" = res_unique,
              "manual_dedup" = res_manual))

}

