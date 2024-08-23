####------ Deduplicate citations function ------ ####

#'
#' This function deduplicates citation data
#' @details The following fields will be used in `raw_citations` (if provided): record_id, author, year, journal, doi, title, pages, volume, number, abstract, isbn, label, source
#' @import dplyr
#' @import progressr
#' @import shiny
#' @importFrom rlang :=
#' @importFrom utils menu
#' @param raw_citations A dataframe containing duplicate ciations
#' @param manual_dedup Logical value. Do you want to retrieve dataframe for manual deduplication?
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @param user_input Do you want to proceed if important columns are missing? 1-yes; 2-no
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return A list of 2 dataframes - unique citations and citations to be manually deduplicated if option selected
#' @export
#'
#' @examples
#'
#' # Perform deduplication
#' result <- dedup_citations(citations_df, keep_source="Embase")
#'
#' # View unique citations
#' head(result$unique)
#'
#'
dedup_citations <- function(raw_citations, manual_dedup = TRUE,
                            merge_citations=TRUE, keep_source=NULL, keep_label=NULL, extra_merge_fields = NULL,
                            show_unknown_tags=TRUE, user_input=NA) {

  if ("shiny" %in% .packages(all.available=TRUE)) {
    shiny_progress <- shiny::isRunning()
  } else {
    shiny_progress <- FALSE
  }

  # reformat if dedup is running again on an already deduplicated dataset (rerun)
  raw_citations <- format_rerun(raw_citations)

  # for R shiny app
  if(shiny_progress == TRUE){

    res <- withProgress(message = "formatting data...", value = 0, {

      if(show_unknown_tags == TRUE){

        # add unknowns for blanks and NAs
        raw_citations <- raw_citations  %>%
          mutate(across(where(is.character), ~ na_if(.,""))) %>%
          mutate(label = ifelse(is.na(.data$label), "unknown", paste(.data$label))) %>%
          mutate(source = ifelse(is.na(.data$source), "unknown", paste(.data$source))) %>%
          mutate(across({{extra_merge_fields}}, ~ replace(., is.na(.), "unknown")))

      } else {

        # add NA for blanks
        raw_citations <- raw_citations  %>%
          mutate(across(where(is.character), ~ na_if(.,"")))
      }

      # add misisng cols, give warnings for cols not included
      raw_citations <- add_missing_cols(raw_citations)

      if(manual_dedup == TRUE){
        # to return empty data frame in case there are no duplicates
        res <- list("manual_dedup" = data.frame())
      } else {
        res <- list()
      }

      # order citations
      ordered_citations <- order_citations(raw_citations, extra_merge_fields)

      # format citations
      formatted_citations <- format_citations(ordered_citations)
      incProgress(0.2/1, message = "identifying potential duplicates...")

      # find matching pairs
      pairs <- match_citations(formatted_citations)

      # shiny progress update
      incProgress(0.4/1)

      # warnings if no possible duplicates detected
      if(is.null(pairs) || length(pairs$record_id1)==0) {
        warning("No duplicates detected")
        raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
        res$unique <- raw_citations
        return(res)
      }

      # identify pairs that are true and possible pairs
      pair_types <- identify_true_matches(pairs)
      true_pairs <- pair_types$true_pairs

      # shiny progress update
      incProgress(0.5/1)

      # warning if no true duplicates
      if(is.null(true_pairs) || length(true_pairs$record_id1)==0) {
        warning("No duplicates detected!")
        raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
        res$unique <- raw_citations
        return(res)
      }

      # shiny progress update
      incProgress(0.6/1, message = "merging duplicate citations...")

      # generate duplicate identifiers using graph theory
      matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)

      # merge citations (best option) or keep one citations from each
      if(merge_citations == TRUE){
        res$unique <- raw_citations <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
      } else{
        res$unique <- raw_citations <- keep_one_unique_citation(matched_pairs_with_ids)
      }

      # processing possible pairs for manual dedup review
      if(manual_dedup == TRUE){

        # shiny update message
        incProgress(0.8/1, message = "flagging potential pairs for manual dedup...")
        res$manual_dedup <- process_possible_pairs(pair_types$maybe_pairs, ordered_citations, matched_pairs_with_ids, extra_merge_fields)

      }

      # shiny update message
      incProgress(1/1) # Increase progress bar to 100%

      return(res)

    })

    # make sure data is returned ungrouped
    res$unique <- res$unique %>%
      ungroup()

    return(res)

  } else {

    # error handling for missing cols
    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    essential_cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn")
    all_missing_cols <- cols[!(cols %in% colnames(raw_citations))] # find missing columns
    missing_cols <- essential_cols[!(essential_cols %in% colnames(raw_citations))] # find missing columns

    if (length(missing_cols) > 0) {

      message(paste("Warning: The following columns are missing:", paste(missing_cols, collapse = ", ")))

      if (is.na(user_input)) {
        user_input <- utils::menu(c("Yes", "No"),
                                  title= paste("Are you sure you want to proceed?"))
      }

      if (user_input == "1") {

        message("formatting data...")

      } else {
        return("Halting dedup...")}

    } else { message("formatting data...") }

    raw_citations[all_missing_cols] <- NA #set all missing columns to NA

    if(show_unknown_tags == TRUE){

      # add unknowns for blanks and NAs
      raw_citations <- raw_citations  %>%
        mutate(across(where(is.character), ~ na_if(.,""))) %>%
        mutate(label = ifelse(is.na(.data$label), "unknown", paste(.data$label))) %>%
        mutate(source = ifelse(is.na(.data$source), "unknown", paste(.data$source))) %>%
        mutate(across({{extra_merge_fields}}, ~ replace(., is.na(.), "unknown")))
    } else {

      # add unknowns for blanks and NAs
      raw_citations <- raw_citations  %>%
        mutate(across(where(is.character), ~ na_if(.,"")))
    }

    # add misisng cols, give warnings for cols not included
    raw_citations <- add_missing_cols(raw_citations)

    # order citations
    ordered_citations <- order_citations(raw_citations)

    # format citations
    formatted_citations <- format_citations(ordered_citations)

    # user progress message
    message("identifying potential duplicates...")

    # find matching pairs
    pairs <- match_citations(formatted_citations)

    if(manual_dedup == TRUE){
      # to return empty data frame in case there are no duplicates
      res <- list("manual_dedup" = data.frame())
    } else {
      res <- list()
    }

    # warning if no potential duplicates
    if(is.null(pairs) || length(pairs$record_id1)==0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_ids <- raw_citations$record_id
      res$unique <- raw_citations
      return(res)
    }

    # identify true pairs
    pair_types <- identify_true_matches(pairs)
    true_pairs <- pair_types$true_pairs

    # warning if no true duplicates
    if(is.null(true_pairs) || length(true_pairs$record_id1)==0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_ids <- raw_citations$record_id
      res$unique <- raw_citations
      return(res)
    }

    # user update message
    message("identified duplicates!")

    # generate duplicate identifiers
    suppressMessages({
      matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)
    })

    # merge citations or keep one citation from each group
    suppressMessages({

      if(merge_citations == TRUE){

        res$unique <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
      } else{
        res$unique <- keep_one_unique_citation(matched_pairs_with_ids)
      }

    })

    # process possible pairs for manual dedup review
    if(manual_dedup == TRUE){

      message("flagging potential pairs for manual dedup...")
      res$manual_dedup <- process_possible_pairs(pair_types$maybe_pairs, ordered_citations, matched_pairs_with_ids, extra_merge_fields)

    }

    # make sure data is returned ungrouped
    res$unique <- res$unique %>%
      ungroup()

    # calculate results and output messages
    n_unique <- length(unique(res$unique$duplicate_id))
    n_start <- length(unique(formatted_citations$record_id))
    n_dups <- n_start - n_unique

    message(paste(n_start, "citations loaded..."))
    message(paste(n_dups, "duplicate citations removed..."))
    message(paste(n_unique, "unique citations remaining!"))

    return(res)

  }
}
