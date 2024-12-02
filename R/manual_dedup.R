####------ Deduplicate citations WITH manual dups added function ------ ####
#'
#' This function performs additional deduplication with the additional of manually flagged duplicates
#' @export
#' @import dplyr
#' @param unique_citations A dataframe containing citations after automated deduplication
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param additional_pairs dataframe of citations with manual pairs, a subset of the manual pairs export. If a `result` column is included, only those with a value of `match` will be merged
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return Unique citations post manual deduplication
#' @examples
#'
#' # Perform deduplication
#' result <- dedup_citations(citations_df, keep_source="Embase")
#'
#' # View unique citations
#' res_unique <- result$unique
#' head(result$manual_dedup)
#'
#' true_dups <- result$manual_dedup[1:5,]
#' # or equivalently
#' true_dups <- result$manual_dedup
#' true_dups$result[1:5] <- "match"
#'
#' # You can also use a Shiny interface to review the potential duplicates
#' # true_dups <- manual_dedup_shiny(result$manual_dedup)
#'
#'final_result <- dedup_citations_add_manual(res_unique, additional_pairs = true_dups)
#'
dedup_citations_add_manual <- function(unique_citations, merge_citations=TRUE, keep_source=NULL, keep_label=NULL,
                                       additional_pairs, extra_merge_fields = NULL, show_unknown_tags=TRUE){

    if ("result" %in% names(additional_pairs)){
      additional_pairs <- additional_pairs[additional_pairs$result == "match",]
      if (nrow(additional_pairs) == 0){
        message("Beware: if additional_pairs contains a `result` column, only those with a value of `match` will be merged. Currently, this means that there are no pairs to be merged.")
        return(unique_citations)
      }
    }

    unique_citations <- unique_citations %>%
      rename(record_id = duplicate_id)

    res <- generate_dup_id(additional_pairs, unique_citations, keep_source, keep_label)
    unique_citations_with_metadata <- merge_metadata(res, extra_merge_fields)

    return(unique_citations_with_metadata)
  }
