####------ Deduplicate citations function ------ ####

#'
#' This function deduplicates citation data
#' @details The following fields will be used in `raw_citations` (if provided): record_id, author, year, journal, doi, title, pages, volume, number, abstract, isbn, label, source
#' @import dplyr
#' @import progressr
#' @importFrom rlang :=
#' @import utils
#' @param raw_citations A dataframe containing duplicate ciations
#' @param manual_dedup Logical value. Do you want to retrieve dataframe for manual deduplication?
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @param shiny_progress Switch on progress indicators for shiny applications
#' @param user_input Do you want to proceed if important columns are missing? 1-yes; 2-no
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return A list of 2 dataframes - unique citations and citations to be manually deduplicated if option selected
#' @export
dedup_citations <- function(raw_citations, manual_dedup = TRUE,
                            merge_citations=TRUE, keep_source=NULL, keep_label=NULL, extra_merge_fields = NULL,
                            shiny_progress=FALSE, show_unknown_tags=TRUE, user_input=NA) {

  if(shiny_progress == TRUE){

    withProgress(message = "formatting data...", value = 0, {

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

      # add warning for no record id
      if(!"record_id" %in% names(raw_citations)){
        warning("Search does not contain a record_id column. A record_id will be created using row numbers")

        # add record id using row number
        raw_citations <- add_id_citations(raw_citations)

        # add warning for any missing record id
      } else if(any(is.na(raw_citations$record_id)) | any(raw_citations$record_id=="")){
        warning("Search contains missing values for the record_id column. A record_id will be created using row numbers")

        # add record id using row number
        raw_citations <- add_id_citations(raw_citations)

        # add warning for non unique ids
      }  else if(length(unique(raw_citations$record_id)) != nrow(raw_citations)){
        warning("The record_id column is not unique. A record_id will be created using row numbers")

        # add record id using row number
        raw_citations <- add_id_citations(raw_citations)
      }

      cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
      essential_cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn")
      all_missing_cols <- cols[!(cols %in% colnames(raw_citations))] # find missing columns
      missing_cols <- essential_cols[!(essential_cols %in% colnames(raw_citations))] # find missing columns

      if (length(missing_cols) > 0) {
        warning(paste0("The following columns are missing: ", paste(missing_cols, collapse = ", "), "\n"))
        message(paste0("Setting missing cols to NA"))
      }
      raw_citations[all_missing_cols] <- NA # set missing columns to NA

      raw_citations$record_id <- as.character(raw_citations$record_id)
      ordered_citations <- order_citations(raw_citations, extra_merge_fields)
      formatted_citations <- format_citations(ordered_citations)

      incProgress(0.2/1, message = "identifying potential duplicates...")

      # find matching pairs
      pairs <- match_citations(formatted_citations)

      incProgress(0.4/1)

      # warning if no duplicates
      if(is.null(pairs)) {
        warning("No duplicates detected")
        raw_citations$duplicate_id <- raw_citations$record_id
        return(list("unique" = raw_citations))
      }

      if(length(pairs$record_id1)==0) {
        warning("No duplicates detected")
        raw_citations$duplicate_id <- raw_citations$record_id
        return(list("unique" = raw_citations))
      }

      pair_types <- identify_true_matches(pairs)
      true_pairs <- pair_types$true_pairs

      incProgress(0.5/1)

      # warning if no duplicates
      if(is.null(true_pairs)) {
        warning("No duplicates detected!")
        raw_citations$duplicate_id <- raw_citations$record_id
        return(list("unique" = raw_citations))
      }

      if(length(true_pairs$record_id1)==0) {
        warning("No duplicates detected!")
        raw_citations$duplicate_id <- raw_citations$record_id
        return(list("unique" = raw_citations))
      }

      incProgress(0.6/1, message = "merging duplicate citations...")

      matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)


      if(merge_citations == TRUE){

        unique_citations_with_metadata <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
      } else{
        unique_citations_with_metadata <- keep_one_unique_citation(matched_pairs_with_ids)

      }

      if(manual_dedup == TRUE){

        incProgress(0.8/1, message = "flagging potential pairs for manual dedup...")

        maybe_pairs <- pair_types$maybe_pairs

        maybe_pairs <- maybe_pairs  %>%
          mutate(author1 =ordered_citations$author[id1]) %>%
          mutate(author2 =ordered_citations$author[id2]) %>%
          mutate(title1 =ordered_citations$title[id1]) %>%
          mutate(title2 =ordered_citations$title[id2]) %>%
          mutate(abstract1 =ordered_citations$abstract[id1]) %>%
          mutate(abstract2 =ordered_citations$abstract[id2]) %>%
          mutate(doi1= ordered_citations$doi[id1]) %>%
          mutate(doi2 =ordered_citations$doi[id2]) %>%
          mutate(year1=ordered_citations$year[id1]) %>%
          mutate(year2=ordered_citations$year[id2]) %>%
          mutate(number1 =ordered_citations$number[id1]) %>%
          mutate(number2 =ordered_citations$number[id2]) %>%
          mutate(pages1 =ordered_citations$pages[id1]) %>%
          mutate(pages2 =ordered_citations$pages[id2]) %>%
          mutate(volume1 =ordered_citations$volume[id1]) %>%
          mutate(volume2 =ordered_citations$volume[id2]) %>%
          mutate(journal1 =ordered_citations$journal[id1]) %>%
          mutate(journal2 =ordered_citations$journal[id2]) %>%
          mutate(isbn1 =ordered_citations$isbn[id1]) %>%
          mutate(isbn2 =ordered_citations$isbn[id2]) %>%
          mutate(record_id1=ordered_citations$record_id[id1]) %>%
          mutate(record_id2 =ordered_citations$record_id[id2]) %>%
          mutate(label1 =ordered_citations$label[id1]) %>%
          mutate(label2 =ordered_citations$label[id2]) %>%
          mutate(source1 =ordered_citations$source[id1]) %>%
          mutate(source2 =ordered_citations$source[id2])

        if(!is.null(extra_merge_fields)){

          maybe_pairs <- maybe_pairs  %>%
            mutate(!!paste0(extra_merge_fields, 1) := ordered_citations[[extra_merge_fields]][id1],
                   !!paste0(extra_merge_fields, 2) := ordered_citations[[extra_merge_fields]][id2]) %>%
            select(author1, author2, author, title1,
                   title2, title, abstract1, abstract2, abstract, year1,
                   year2, year, number1, number2, number, pages1, pages2,
                   pages, volume1, volume2, volume, journal1, journal2,
                   journal, isbn, isbn1, isbn2, doi1, doi2, doi,
                   record_id1, record_id2, label1,
                   label2, source1, source2, starts_with(paste0(extra_merge_fields)))

        } else {

          maybe_pairs <- maybe_pairs  %>%
            select(author1, author2, author, title1,
                   title2, title, abstract1, abstract2, abstract, year1,
                   year2, year, number1, number2, number, pages1, pages2,
                   pages, volume1, volume2, volume, journal1, journal2,
                   journal, isbn, isbn1, isbn2, doi1, doi2, doi,
                   record_id1, record_id2, label1,
                   label2, source1, source2)

        }

        ids <- matched_pairs_with_ids %>%
          select(duplicate_id, record_id)

        maybe_pairs <- left_join(maybe_pairs, ids, by=c("record_id1"="record_id"))
        maybe_pairs <- left_join(maybe_pairs, ids, by=c("record_id2"="record_id"))

        maybe_pairs <- maybe_pairs %>%
          mutate(match= ifelse(duplicate_id.x == duplicate_id.y, TRUE, FALSE)) %>%
          filter(match == FALSE) %>%
          group_by(.data$duplicate_id.x, .data$duplicate_id.y) %>%
          slice_head() %>%
          ungroup()

        if(length(maybe_pairs$record_id1)==0){
          manual_dedup_df <- maybe_pairs
        } else {

          unique_pairs <- maybe_pairs %>%
            select(duplicate_id.x, duplicate_id.y) %>%
            rowwise() %>%
            mutate(min_id = min(duplicate_id.x, duplicate_id.y),
                   max_id = max(duplicate_id.x, duplicate_id.y)) %>%
            group_by(min_id, max_id) %>%
            slice_head() %>%
            mutate(unique = TRUE)

          maybe_pairs <- maybe_pairs %>%
            left_join(unique_pairs) %>%
            filter(!is.na(unique)) %>%
            select(-unique)

          manual_dedup_df <- maybe_pairs
        }


      }

      # make sure data is returned ungrouped
      unique_citations_with_metadata <- unique_citations_with_metadata %>%
        ungroup()


      n_unique <- length(unique(unique_citations_with_metadata$duplicate_id))
      n_start <- length(unique(formatted_citations$record_id))
      n_dups <- n_start - n_unique


      message(paste(n_start, "citations loaded..."))
      message(paste(n_dups, "duplicate citations removed..."))
      message(paste(n_unique, "unique citations remaining!"))

      incProgress(1/1) # Increase progress bar to 100%
    })

    return(list("unique" = unique_citations_with_metadata,
                "manual_dedup" = manual_dedup_df))
  } else {

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    essential_cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn")
    all_missing_cols <- cols[!(cols %in% colnames(raw_citations))] # find missing columns
    missing_cols <- essential_cols[!(essential_cols %in% colnames(raw_citations))] # find missing columns

    if (length(missing_cols) > 0) {

      message(paste("Warning: The following columns are missing:", paste(missing_cols, collapse = ", ")))

      if (is.na(user_input)) {
        user_input <- menu(c("Yes", "No"),
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

    # add warning for no record id
    if(!"record_id" %in% names(raw_citations)){
      warning("Search does not contain a record_id column. A record_id will be created using row numbers")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)

      # add warning for any missing record id
    } else if(any(is.na(raw_citations$record_id)) | any(raw_citations$record_id=="")){
      warning("Search contains missing values for the record_id column. A record_id will be created using row numbers")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)

      # add warning for non unique ids
    }  else if(length(unique(raw_citations$record_id)) != nrow(raw_citations)){
      warning("The record_id column is not unique. A record_id will be created using row numbers")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    essential_cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn")
    all_missing_cols <- cols[!(cols %in% colnames(raw_citations))] # find missing columns
    missing_cols <- essential_cols[!(essential_cols %in% colnames(raw_citations))] # find missing columns

    if (length(missing_cols) > 0) {
      warning(paste0("The following columns are missing: ", paste(missing_cols, collapse = ", "), "\n"))
      message(paste0("Setting missing cols to NA"))
    }
    raw_citations[all_missing_cols] <- NA # set missing columns to NA

    raw_citations$record_id <- as.character(raw_citations$record_id)
    ordered_citations <- order_citations(raw_citations)
    formatted_citations <- format_citations(ordered_citations)

    message("identifying potential duplicates...")

    # find matching pairs
    pairs <- match_citations(formatted_citations)

    # warning if no duplicates
    if(is.null(pairs)) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_id
      return(list("unique" = raw_citations))
    }

    if(length(pairs$record_id1)==0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_id
      return(list("unique" = raw_citations))
    }

    pair_types <- identify_true_matches(pairs)
    true_pairs <- pair_types$true_pairs

    # warning if no duplicates
    if(is.null(true_pairs)) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_id
      return(list("unique" = raw_citations))
    }

    if(length(true_pairs$record_id1)==0) {
      warning("No duplicates detected!")
      raw_citations$duplicate_id <- raw_citations$record_id
      return(list("unique" = raw_citations))
    }

    message("identified duplicates!")
    matched_pairs_with_ids <- generate_dup_id(true_pairs, raw_citations, keep_source, keep_label)


    if(merge_citations == TRUE){

      unique_citations_with_metadata <- merge_metadata(matched_pairs_with_ids, extra_merge_fields)
    } else{
      unique_citations_with_metadata <- keep_one_unique_citation(matched_pairs_with_ids)

    }


    if(manual_dedup == TRUE){

      message("flagging potential pairs for manual dedup...")

      maybe_pairs <- pair_types$maybe_pairs

      maybe_pairs <- maybe_pairs  %>%
        mutate(author1 =ordered_citations$author[id1]) %>%
        mutate(author2 =ordered_citations$author[id2]) %>%
        mutate(title1 =ordered_citations$title[id1]) %>%
        mutate(title2 =ordered_citations$title[id2]) %>%
        mutate(abstract1 =ordered_citations$abstract[id1]) %>%
        mutate(abstract2 =ordered_citations$abstract[id2]) %>%
        mutate(doi1= ordered_citations$doi[id1]) %>%
        mutate(doi2 =ordered_citations$doi[id2]) %>%
        mutate(year1=ordered_citations$year[id1]) %>%
        mutate(year2=ordered_citations$year[id2]) %>%
        mutate(number1 =ordered_citations$number[id1]) %>%
        mutate(number2 =ordered_citations$number[id2]) %>%
        mutate(pages1 =ordered_citations$pages[id1]) %>%
        mutate(pages2 =ordered_citations$pages[id2]) %>%
        mutate(volume1 =ordered_citations$volume[id1]) %>%
        mutate(volume2 =ordered_citations$volume[id2]) %>%
        mutate(journal1 =ordered_citations$journal[id1]) %>%
        mutate(journal2 =ordered_citations$journal[id2]) %>%
        mutate(isbn1 =ordered_citations$isbn[id1]) %>%
        mutate(isbn2 =ordered_citations$isbn[id2]) %>%
        mutate(record_id1=ordered_citations$record_id[id1]) %>%
        mutate(record_id2 =ordered_citations$record_id[id2]) %>%
        mutate(label1 =ordered_citations$label[id1]) %>%
        mutate(label2 =ordered_citations$label[id2]) %>%
        mutate(source1 =ordered_citations$source[id1]) %>%
        mutate(source2 =ordered_citations$source[id2])

      if(!is.null(extra_merge_fields)){

        maybe_pairs <- maybe_pairs  %>%
          mutate(!!paste0(extra_merge_fields, 1) := ordered_citations[[extra_merge_fields]][id1],
                 !!paste0(extra_merge_fields, 2) := ordered_citations[[extra_merge_fields]][id2]) %>%
          select(author1, author2, author, title1,
                 title2, title, abstract1, abstract2, abstract, year1,
                 year2, year, number1, number2, number, pages1, pages2,
                 pages, volume1, volume2, volume, journal1, journal2,
                 journal, isbn, isbn1, isbn2, doi1, doi2, doi,
                 record_id1, record_id2, label1,
                 label2, source1, source2, starts_with(paste0(extra_merge_fields)))

      }  else {

        maybe_pairs <- maybe_pairs  %>%
          select(author1, author2, author, title1,
                 title2, title, abstract1, abstract2, abstract, year1,
                 year2, year, number1, number2, number, pages1, pages2,
                 pages, volume1, volume2, volume, journal1, journal2,
                 journal, isbn, isbn1, isbn2, doi1, doi2, doi,
                 record_id1, record_id2, label1,
                 label2, source1, source2)

      }

      ids <- matched_pairs_with_ids %>%
        select(duplicate_id, record_id)

      maybe_pairs <- left_join(maybe_pairs, ids, by=c("record_id1"="record_id"))
      maybe_pairs <- left_join(maybe_pairs, ids, by=c("record_id2"="record_id"))

      maybe_pairs <- maybe_pairs %>%
        mutate(match= ifelse(duplicate_id.x == duplicate_id.y, TRUE, FALSE)) %>%
        filter(match == FALSE) %>%
        group_by(.data$duplicate_id.x, .data$duplicate_id.y) %>%
        slice_head() %>%
        ungroup()

      if(length(maybe_pairs$record_id1)==0){
        manual_dedup_df <- maybe_pairs
      } else {

        unique_pairs <- maybe_pairs %>%
          select(duplicate_id.x, duplicate_id.y) %>%
          rowwise() %>%
          mutate(min_id = min(duplicate_id.x, duplicate_id.y),
                 max_id = max(duplicate_id.x, duplicate_id.y)) %>%
          group_by(min_id, max_id) %>%
          slice_head() %>%
          mutate(unique = TRUE)

        maybe_pairs <- maybe_pairs %>%
          left_join(unique_pairs) %>%
          filter(!is.na(unique)) %>%
          select(-unique)

        manual_dedup_df <- maybe_pairs
      }

    }

    # make sure data is returned ungrouped
    unique_citations_with_metadata <- unique_citations_with_metadata %>%
      ungroup()


    n_unique <- length(unique(unique_citations_with_metadata$duplicate_id))
    n_start <- length(unique(formatted_citations$record_id))
    n_dups <- n_start - n_unique


    message(paste(n_start, "citations loaded..."))
    message(paste(n_dups, "duplicate citations removed..."))
    message(paste(n_unique, "unique citations remaining!"))

    if(manual_dedup == TRUE){

      return(list("unique" = unique_citations_with_metadata,
                  "manual_dedup" = manual_dedup_df))
    } else {

      return(unique_citations_with_metadata)
    }

  }
}
