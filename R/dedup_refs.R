####------ Assign id ------ ####

#' This function adds an id to citation data if missing
#' @param raw_citations Citation dataframe with relevant columns
#' @return Dataframe of citations with id
#' @import dplyr
#' @noRd
add_id_citations <- function(raw_citations){

  # add record id from row number if missing
  raw_citations <- raw_citations %>%
    mutate(record_id =  as.character(row_number()+1000))

}

#' This function orders citation data for deduplication
#' @inherit raw_citations
#' @return Dataframe of ordered citations with id
#' @import dplyr
#' @import utf8
#' @noRd
order_citations <- function(raw_citations, extra_merge_fields){
  # arrange by Year and presence of an Abstract - we want to keep newer records and records with an abstract preferentially

  ordered_citations <- raw_citations %>%
    arrange(abstract, year) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8

  # select relevant columns
  ordered_citations <- ordered_citations  %>%
    dplyr::select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label, source, {{extra_merge_fields}})


  return(ordered_citations)
}

####------ Format citation data ------ ####

#' This function formats citation data for deduplication
#' @inherit raw_citations
#' @return Dataframe of formatted citations with id
#' @import dplyr
#' @noRd
format_citations <- function(raw_citations){

  # make sure author is a character
  raw_citations$author <- as.character(raw_citations$author)

  # Fix author formatting so similar
  raw_citations <- raw_citations %>%
    mutate(author = ifelse(.data$author=="", "Unknown", .data$author)) %>%
    mutate(author = ifelse(is.na(.data$author), "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="Anonymous", "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="Anonymous.", "Unknown", .data$author)) %>%
    mutate(author = ifelse(.data$author=="[Anonymous] A", "Unknown", .data$author)) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8

  # Fix page formatting
  raw_citations$pages <- lapply(raw_citations$pages, function(x) gsub("--", "-", x))

  # Make all upper case by selecting cols in order and formatting all metadata to upper
  # Note that source, label and record id are retained - important for joining later
  formatted_citations <- raw_citations %>%
    select(everything(), source, label, record_id)

  ncol(formatted_citations)
  to_col <- ncol(formatted_citations) - 3

  formatted_citations[,1:to_col] <- as.data.frame(sapply(formatted_citations[,1:to_col], toupper))

  # get rid of punctuation and differnces in doi formatting
  formatted_citations["doi"] <- sapply(formatted_citations["doi"], function(x) gsub("%28", "(", x))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("%29", ")", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI: ", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI:", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI", "", x)))

  formatted_citations["title"] <- as.data.frame(sapply(formatted_citations["title"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["year"] <- as.data.frame(sapply(formatted_citations["year"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["abstract"] <- as.data.frame(sapply(formatted_citations["abstract"], function(x) gsub("[[:punct:]]", "", x)))

  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  formatted_citations<-formatted_citations %>%
    filter(!is.na(record_id))

  # sort out NA / missing data formatting for optimal matching
  formatted_citations <- formatted_citations %>%
    mutate(author = ifelse(.data$author=="NA", NA, paste(.data$author))) %>%
    mutate(year = ifelse(.data$year=="NA", NA, paste(.data$year))) %>%
    mutate(title = ifelse(.data$title=="NA", NA, paste(.data$title))) %>%
    mutate(number = ifelse(.data$number=="NA", NA, paste(.data$number))) %>%
    mutate(volume = ifelse(.data$volume=="NA", NA, paste(.data$volume))) %>%
    mutate(pages = ifelse(.data$pages=="NA", NA, paste(.data$pages))) %>%
    mutate(abstract = ifelse(.data$abstract=="NA", NA, paste(.data$abstract))) %>%
    mutate(doi = ifelse(.data$doi=="NA", NA, paste(doi))) %>%
    mutate(journal = ifelse(.data$journal=="NA", NA, paste(.data$journal))) %>%
    mutate(isbn = ifelse(.data$isbn=="", NA, paste(.data$isbn)))

  formatted_citations<- formatted_citations %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, source, label)

  formatted_citations$record_id <- as.character(formatted_citations$record_id)

  return(formatted_citations)

}

####------ Identify all possible matching pairs of citations ------ ####

#' This function identifies matching pairs of citations
#' @param formatted_citations Formatted citation dataframe with relevant columns and id column
#' @return Dataframe of citation pairs
#' @import RecordLinkage
#' @import parallel
#' @import parallelly
#' @noRd
match_citations <- function(formatted_citations){

  # ROUND 1: run compare.dedup function and block by title&pages OR title&author OR title&abstract OR doi
  try(newpairs <- compare.dedup(formatted_citations, blockfld = list(c(2,8), c(1,2), c(2,5), 6), exclude=c("record_id", "source", "label")), silent=TRUE)

  # Create df of pairs
  try(linkedpairs <- as.data.frame(newpairs$pairs), silent=TRUE)

  # ROUND 2: run compare.dedup function and block by author&year&pages OR journal&volume&pages or isbn&volume&pages OR title&isbn
  try(newpairs2 <- compare.dedup(formatted_citations, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), exclude= c("record_id", "source", "label")), silent=TRUE)

  #Create df of pairs
  try(linkedpairs2 <- as.data.frame(newpairs2$pairs), silent=TRUE)

  # ROUND 3: run compare.dedup function and block by year&pages&volume OR year&number&volume or year&pages&number
  try(newpairs3 <- compare.dedup(formatted_citations, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), exclude=c("record_id", "source", "label")), silent = TRUE)

  #Create df of pairs
  try(linkedpairs3 <- as.data.frame(newpairs3$pairs), silent=TRUE)

  # ROUND 4: run compare.dedup function and block by author&year OR year&title OR title&volume OR title&journal
  try(newpairs4 <- compare.dedup(formatted_citations, blockfld = list(c(1,3), c(3,2),c(2,9), c(2,4)), exclude=c("record_id", "source", "label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs4 <- as.data.frame(newpairs4$pairs), silent=TRUE)

  # Combine all possible pairs
  pairs <- rbind(get0("linkedpairs"),
                 get0("linkedpairs2"),
                 get0("linkedpairs3"),
                 get0("linkedpairs4"))

  pairs <- unique(pairs)

  if(is.null(pairs)){

    return()
  }

  # Obtain metadata for matching pairs
  pairs <- pairs  %>%
    mutate(author1 =formatted_citations$author[id1]) %>%
    mutate(author2 =formatted_citations$author[id2]) %>%
    mutate(title1 =formatted_citations$title[id1]) %>%
    mutate(title2 =formatted_citations$title[id2]) %>%
    mutate(abstract1 =formatted_citations$abstract[id1]) %>%
    mutate(abstract2 =formatted_citations$abstract[id2]) %>%
    mutate(doi1= formatted_citations$doi[id1]) %>%
    mutate(doi2 =formatted_citations$doi[id2]) %>%
    mutate(year1=formatted_citations$year[id1]) %>%
    mutate(year2=formatted_citations$year[id2]) %>%
    mutate(number1 =formatted_citations$number[id1]) %>%
    mutate(number2 =formatted_citations$number[id2]) %>%
    mutate(pages1 =formatted_citations$pages[id1]) %>%
    mutate(pages2 =formatted_citations$pages[id2]) %>%
    mutate(volume1 =formatted_citations$volume[id1]) %>%
    mutate(volume2 =formatted_citations$volume[id2]) %>%
    mutate(journal1 =formatted_citations$journal[id1]) %>%
    mutate(journal2 =formatted_citations$journal[id2]) %>%
    mutate(isbn1 =formatted_citations$isbn[id1]) %>%
    mutate(isbn2 =formatted_citations$isbn[id2]) %>%
    mutate(record_id1=formatted_citations$record_id[id1]) %>%
    mutate(record_id2 =formatted_citations$record_id[id2]) %>%
    mutate(label1 =formatted_citations$label[id1]) %>%
    mutate(label2 =formatted_citations$label[id2]) %>%
    mutate(source1 =formatted_citations$source[id1]) %>%
    mutate(source2 =formatted_citations$source[id2])

  pairs <- pairs %>%
    select(id1, id2, author1, author2, author, title1, title2, title, abstract1, abstract2, abstract, year1, year2, year, number1, number2, number, pages1, pages2, pages, volume1, volume2, volume, journal1, journal2, journal, isbn, isbn1, isbn2, doi1, doi2, doi, record_id1, record_id2, label1, label2, source1, source2)

  numCores <- parallelly::availableCores()
  numCores

  if (.Platform$OS.type != "unix") {
    try(pairs$author <- mapply(jarowinkler, pairs$author1, pairs$author2), silent = TRUE)
    try(pairs$title <-mapply(jarowinkler, pairs$title1, pairs$title2), silent = TRUE)
    try(pairs$abstract <- mapply(jarowinkler, pairs$abstract1, pairs$abstract2), silent = TRUE)
    try(pairs$year <- mapply(jarowinkler, pairs$year1, pairs$year2), silent = TRUE)
    try(pairs$pages <- mapply(jarowinkler, pairs$pages1, pairs$pages2), silent = TRUE)
    try(pairs$number <- mapply(jarowinkler, pairs$number1, pairs$number2), silent = TRUE)
    try(pairs$volume <- mapply(jarowinkler, pairs$volume1, pairs$volume2), silent = TRUE)
    try(pairs$journal <- mapply(jarowinkler, pairs$journal1, pairs$journal2), silent = TRUE)
    try(pairs$isbn <- mapply(jarowinkler, pairs$isbn1, pairs$isbn2), silent = TRUE)
    try(pairs$doi <- mapply(jarowinkler, pairs$doi1, pairs$doi2), silent = TRUE)

  } else{

    suppressWarnings(try(pairs$author <- parallel::mcmapply(jarowinkler, pairs$author1, pairs$author2, mc.cores = numCores), silent = TRUE))
    suppressWarnings(try(pairs$title <- parallel::mcmapply(jarowinkler, pairs$title1, pairs$title2, mc.cores = numCores), silent = TRUE))
    suppressWarnings(try(pairs$abstract <- parallel::mcmapply(jarowinkler, pairs$abstract1, pairs$abstract2, mc.cores = numCores), silent = TRUE))
    suppressWarnings(try(pairs$year <- ::mapply(jarowinkler, pairs$year1, pairs$year2), silent = TRUE))
    suppressWarnings(try(pairs$pages <- mapply(jarowinkler, pairs$pages1, pairs$pages2), silent = TRUE))
    suppressWarnings(try(pairs$number <- mapply(jarowinkler, pairs$number1, pairs$number2), silent = TRUE))
    suppressWarnings(try(pairs$volume <- mapply(jarowinkler, pairs$volume1, pairs$volume2), silent = TRUE))
    suppressWarnings(try(pairs$journal <- parallel::mcmapply(jarowinkler, pairs$journal1, pairs$journal2, mc.cores = numCores), silent = TRUE))
    suppressWarnings(try(pairs$isbn <- parallel::mcmapply(jarowinkler, pairs$isbn1, pairs$isbn2, mc.cores = numCores), silent = TRUE))
    suppressWarnings(try(pairs$doi <- parallel::mcmapply(jarowinkler, pairs$doi1, pairs$doi2, mc.cores = numCores), silent = TRUE))

  }

  pairs <- pairs %>%
    mutate(abstract = ifelse(is.na(.data$abstract1) & is.na(.data$abstract2), 0, .data$abstract)) %>%
    mutate(pages = ifelse(is.na(.data$pages1) & is.na(.data$pages2), 1, .data$pages)) %>%
    mutate(volume = ifelse(is.na(.data$volume1) & is.na(.data$volume2), 1, .data$volume)) %>%
    mutate(number = ifelse(is.na(.data$number1) & is.na(.data$number2), 1, .data$number)) %>%
    mutate(doi = ifelse(is.na(.data$doi1) & is.na(.data$doi2), 0, doi)) %>%
    mutate(isbn = ifelse(is.na(.data$isbn1) & is.na(.data$isbn2), 0, .data$isbn)) %>%
    mutate(year = ifelse(is.na(.data$year1) & is.na(.data$year2), 0, .data$year)) %>%
    mutate(journal = ifelse(is.na(.data$journal1) & is.na(.data$journal2), 0, .data$journal))

}

####------ Identify true duplicate pairs ------ ####

#' This function identifies true pairs from matching pairs of citations and pairs which may be duplicates - for manual deduplication
#' @param pairs citation matches which may be duplicates
#' @return Dataframe of true citation pairs
#' @noRd
identify_true_matches <- function(pairs){

  # filter matching pairs - retain correct matches
  true_pairs <- pairs %>%
    filter(
      (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.90 & author>0.50 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.90 & author>0.50 & journal>0.6) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.50 & journal>0.6) |
        (volume >0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.50  & journal>0.6) |

        (volume >0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.8) |
        (volume>0.8 & pages>0.8 & title>0.90 & abstract>0.9 & author>0.8) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.9 & author>0.8) |

        (doi>0.95 & author>0.75 & title>0.9) |

        (title>0.80 & abstract>0.90 & volume>0.85 & journal>0.65 & author>0.9) |
        (title>0.90 & abstract>0.80 & volume>0.85 & journal>0.65 & author>0.9)|

        (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.8 & author>0.9 & journal>0.75) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.80 & author>0.9 & journal>0.75) |
        (volume>0.8 & number>0.8 & title>0.90 & abstract>0.8 & author>0.9  & journal>0.75) |

        (title>0.9 & author>0.9 & abstract>0.9 & journal >0.7)|
        (title>0.9 & author>0.9 & abstract>0.9 & isbn >0.99)|

        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & journal>0.6) |
        (number>0.9 & volume>0.9 & title>0.90 & author>0.90 & isbn>0.99) |
        (pages>0.9 & volume>0.9 & title>0.90 & author>0.80 & journal>0.6) |
        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & isbn>0.99) |

        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & journal>0.9) |
        (number>0.8 & volume>0.8 & title>0.95 & author>0.80 & journal>0.9)|
        (number>0.8 & pages>0.8 & title>0.95 & author>0.80 & journal>0.9) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99))


  # Find papers with low matching dois - often indicates FALSE positive matches
  true_pairs_mismatch_doi <- true_pairs %>%
    filter(!(is.na(doi)| doi ==0 | doi > 0.99)) %>%
    filter(!(title > 0.9 & abstract > 0.9 & (journal|isbn > 0.9)))

  # Remove papers with low matching dois from filtered matched
  true_pairs <- true_pairs %>%
    filter(is.na(doi)| doi > 0.99 | doi == 0 | (title > 0.9 & abstract>0.9 & (journal|isbn > 0.9)))

  true_pairs <- unique(true_pairs)

  # Make year numeric, then find matches where year differs
  true_pairs$year1 <- as.numeric(as.character(true_pairs$year1))
  true_pairs$year2 <- as.numeric(as.character(true_pairs$year2))
  year_mismatch <- true_pairs[which(true_pairs$year1 != true_pairs$year2),]
  year_mismatch_minor1 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2+1 ),]
  year_mismatch_minor2 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2-1 ),]

  year_mismatch_minor <- rbind(year_mismatch_minor1,year_mismatch_minor2)
  year_mismatch_minor <- unique(year_mismatch_minor)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  year_mismatch_major <- year_mismatch[which(!rownames(year_mismatch) %in% rownames(year_mismatch_minor)),]

  true_pairs <- true_pairs[which(!rownames(true_pairs) %in% rownames(year_mismatch_major)),]

  true_pairs <- unique(true_pairs)

  # Get potential duplicates for manual deduplication
  maybe_pairs <- pairs %>%
    filter(title>0.85 & author>0.75 |
             title>0.80 & abstract>0.80 |
             title>0.80 & isbn>0.99 |
             title>0.80 & journal>0.80) %>%
    filter(doi > 0.99 | doi == 0 | is.na(doi)) %>%
    filter(!(as.numeric(year1) - as.numeric(year2) >1)) %>%
    filter(!(as.numeric(year2) - as.numeric(year1) >1))

  maybe_pairs$record_id1 <- as.character(maybe_pairs$record_id1)
  maybe_pairs$record_id2 <- as.character(maybe_pairs$record_id2)
  true_pairs$record_id1 <- as.character(true_pairs$record_id1)
  true_pairs$record_id2 <- as.character(true_pairs$record_id2)

  # get pairs required for manual dedup which are not in true pairs
  maybe_pairs <- anti_join(maybe_pairs, true_pairs, by = c("record_id1", "record_id2"))

  # Add in problem doi matching pairs and different year data in ManualDedup
  important_mismatch <- rbind(true_pairs_mismatch_doi, year_mismatch_major)
  maybe_pairs <- rbind(maybe_pairs, important_mismatch)
  maybe_pairs <- unique(maybe_pairs)

  true_pairs <- true_pairs %>%
    select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2,
           record_id1, record_id2)

  return(list("true_pairs" = true_pairs,
              "maybe_pairs" = maybe_pairs))

}

####------ Generate duplicate ID ------ ####

#' This function generates a duplicate ID for sets of matching citations
#' @param true_pairs citation matches which are true duplicates'
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param formatted_citations formatted citation data
#' @return Dataframe of formatted citation data with duplicate id
#' @import dplyr
#' @noRd
generate_dup_id <- function(true_pairs, raw_citations, keep_source, keep_label){

  # get df of duplicate ids and record ids
  true_pairs_small <- true_pairs %>%
    select(record_id1, record_id2) %>%
    unique()

  # Create a graph from the Edges1 DataFrame
  g <- igraph::graph_from_data_frame(true_pairs_small, directed = FALSE)

  # Get the connected components of the graph
  cc <- igraph::components(g)

  # Add a new column to the Edges1 DataFrame with the component ID for each row
  true_pairs_small$ComponentID <- cc$membership[match(true_pairs_small$record_id1, names(cc$membership))]

  # Get the unique component IDs
  uniqueIDs <- unique(true_pairs_small$ComponentID)

  raw_citations <- raw_citations %>%
    mutate(record_id = as.character(.data$record_id))

  # make character
  duplicate_id <- true_pairs_small %>%
    group_by(ComponentID) %>%
    tidyr::unite(record_ids, record_id1, record_id2, sep = ", ") %>%
    summarise(record_id = paste(record_ids, collapse = ", ")) %>%
    tidyr::separate_rows(record_id, sep= ", ") %>%
    distinct()

  duplicate_id <- duplicate_id %>%
    right_join(raw_citations) %>%
    mutate(ComponentID = ifelse(is.na(ComponentID), paste0(max(duplicate_id$ComponentID)+row_number()), ComponentID))

  if(!is.null(keep_label)){

    order <- c(unique(raw_citations$label))
    chosen_label <- order == keep_label
    order <- c(order[chosen_label], order[!chosen_label])

    citations_duplicate_id <- duplicate_id %>%
      group_by(.data$ComponentID) %>%
      arrange(factor(.data$label, levels = order)) %>%
      mutate(duplicate_id = first(.data$record_id)) %>%
      ungroup() %>%
      select(-ComponentID)


  }

  else if(!is.null(keep_source)){

    order <- c(unique(raw_citations$source))
    chosen_source <- order == keep_source
    order <- c(order[chosen_source], order[!chosen_source])


    citations_duplicate_id <- duplicate_id %>%
      group_by(.data$ComponentID) %>%
      arrange(factor(.data$source, levels = order)) %>%
      mutate(duplicate_id = first(.data$record_id)) %>%
      ungroup() %>%
      select(-ComponentID)

  }

  else{

    citations_duplicate_id <- duplicate_id %>%
      group_by(.data$ComponentID) %>%
      arrange(.data$record_id) %>%
      mutate(duplicate_id = first(.data$record_id)) %>%
      ungroup() %>%
      select(-ComponentID)
  }

  return(citations_duplicate_id)

}

####------ Remove duplicates------ ####

#' This function retains one citation in a set of matching records
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @return Dataframe of citation data with duplicate citation rows removed
#' @import dplyr
#' @noRd
keep_one_unique_citation <- function(true_pairs_with_ids){

  true_pairs_with_ids <- true_pairs_with_ids %>%
    group_by(duplicate_id) %>%
    slice_head()

}

#' This function generates a duplicate ID for sets of matching citations
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @return Dataframe of formatted citation data with duplicate id
#' @import dplyr
merge_metadata <- function(matched_pairs_with_ids, extra_merge_fields){

  if(!is.null(extra_merge_fields)){

    all_metadata_with_duplicate_id <- matched_pairs_with_ids %>%
      mutate_if(is.character, utf8::utf8_encode) %>% # ensure all utf8
      mutate_all(~replace(., .=='NA', NA)) %>% #replace NA
      group_by(.data$duplicate_id) %>% # group by duplicate id
      summarise(across(everything(), ~ trimws(paste(na.omit(.), collapse = ';;;')))) %>% #merge all rows with same dup id, dont merge NA values
      mutate(across(c(everything(), -c(label, source, record_id, {{extra_merge_fields}})), ~ gsub(.x, pattern = ";;;.*", replacement = ""))) %>% #remove extra values in each col, keep first one only
      mutate(across(label, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>%
      mutate(across({{extra_merge_fields}}, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>%
      mutate(across(source, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>%
      mutate(across(record_id, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>% #replace separator to comma
      ungroup() %>%
      rename(record_ids = record_id) %>%
      ungroup()

  } else {

    all_metadata_with_duplicate_id <- matched_pairs_with_ids %>%
      mutate_if(is.character, utf8::utf8_encode) %>% # ensure all utf8
      mutate_all(~replace(., .=='NA', NA)) %>% #replace NA
      group_by(.data$duplicate_id) %>%
      summarise(across(everything(), ~ trimws(paste(na.omit(.), collapse = ';;;')))) %>% #merge all rows with same dup id, dont merge NA values
      mutate(across(c(everything(), -c(label, source, record_id)), ~ gsub(.x, pattern = ";;;.*", replacement = ""))) %>% #remove extra values in each col, keep first one only
      mutate(across(label, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>%
      mutate(across(source, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>%
      mutate(across(record_id, ~ gsub(.x, pattern = ";;;", replacement = ", "))) %>% #replace separator to comma
      ungroup() %>%
      rename(record_ids = record_id) %>%
      ungroup()

  }
}

####------ Deduplicate citations function ------ ####

#'
#' This function deduplicates citation data
#' @export
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
dedup_citations <- function(raw_citations, manual_dedup = TRUE,
                            merge_citations=TRUE, keep_source=NULL, keep_label=NULL, extra_merge_fields = NULL,
                            shiny_progress=FALSE, show_unknown_tags=TRUE, user_input=NA) {

  if(shiny_progress == TRUE){

    withProgress(message = "formatting data...", value = 0, {

      if(show_unknown_tags == TRUE){

        # add unknowns for blanks and NAs
        raw_citations <- raw_citations  %>%
          mutate(across(where(is.character), ~ na_if(.,"")))


      } else {

        # add NA for blanks
        raw_citations <- raw_citations  %>%
          mutate(across(where(is.character), ~ na_if(.,"")))
      }

      if(!is.null(extra_merge_fields)){

        raw_citations <- raw_citations  %>%
          mutate(across(c({{extra_merge_fields}}, label, source), ~ replace(., is.na(.), "unknown")))
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

####------ Deduplicate citations WITH manual dups added function ------ ####
#'
#' This function deduplicates citation data
#' @export
#' @import dplyr
#' @param unique_citations A dataframe containing citations after automated deduplication
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param additional_pairs dataframe of citations with manual pairs, a subset of the manual pairs export
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return Unique citations post added manual deduplication

dedup_citations_add_manual <- function(unique_citations, merge_citations=TRUE, keep_source=NULL, keep_label=NULL,
                                       additional_pairs, extra_merge_fields = NULL, show_unknown_tags=TRUE){

  # if extra merge fields not null
  if(!is.null(extra_merge_fields)){

    extra_cols <- paste(extra_merge_fields, 1:2, sep = "")

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2, !!!extra_cols)


    if (!('record_ids' %in% colnames(unique_citations))) {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_id = ifelse(is.na(.data$duplicate_id.y), .data$record_id, paste0(.data$record_id, ", ", .data$duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(.data$duplicate_id.y), .data$label, paste0(.data$label, ", ", .data$label2))) %>%
        mutate(source = ifelse(is.na(.data$duplicate_id.y), .data$source, paste0(.data$source, ", ", .data$source2))) %>%
        mutate(across(all_of({{extra_merge_fields}}), ~ifelse(is.na(duplicate_id.y), .x, paste0(.x, ", ", get(paste0(cur_column(), 2))))))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, {{extra_merge_fields}}, sep=", ") %>%
        select(-duplicate_id.y, -!!extra_cols) %>%
        unique() %>%
        ungroup() %>%
        group_by(.data$record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations <- unique_citations %>%
        select(-duplicate_id)

      unique_citations_with_metadata <- keep_one_unique_citation(ids)

    } else {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_ids = ifelse(is.na(duplicate_id.y), record_ids, paste0(record_ids, ", ", duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(duplicate_id.y), label, paste0(label, ", ", label2))) %>%
        mutate(source = ifelse(is.na(duplicate_id.y), source, paste0(source, ", ", source2))) %>%
        mutate(across(all_of({{extra_merge_fields}}), ~ifelse(is.na(duplicate_id.y), .x, paste0(.x, ", ", get(paste0(cur_column(), 2))))))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, {{extra_merge_fields}}, sep=", ") %>%
        rename(record_id = record_ids) %>%
        select(-duplicate_id.y, -!!extra_cols) %>%
        unique() %>%
        ungroup() %>%
        group_by(record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations_with_metadata <- merge_metadata(ids, extra_merge_fields)

    }

    unique_citations_with_metadata <-unique_citations_with_metadata %>%
      select(-c(source1, label1, source2, label2))

    return(unique_citations_with_metadata)

  }  else {

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2)

    if (!('record_ids' %in% colnames(unique_citations))) {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_id = ifelse(is.na(.data$duplicate_id.y), .data$record_id, paste0(.data$record_id, ", ", .data$duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(.data$duplicate_id.y), .data$label, paste0(.data$label, ", ", .data$label2))) %>%
        mutate(source = ifelse(is.na(.data$duplicate_id.y), .data$source, paste0(.data$source, ", ", .data$source2)))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, sep=", ") %>%
        select(-duplicate_id.y) %>%
        unique() %>%
        ungroup() %>%
        group_by(.data$record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations <- unique_citations %>%
        select(-duplicate_id)

      unique_citations_with_metadata <- keep_one_unique_citation(ids)

    } else {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_ids = ifelse(is.na(duplicate_id.y), record_ids, paste0(record_ids, ", ", duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(duplicate_id.y), label, paste0(label, ", ", label2))) %>%
        mutate(source = ifelse(is.na(duplicate_id.y), source, paste0(source, ", ", source2)))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, sep=", ") %>%
        rename(record_id = record_ids) %>%
        select(-duplicate_id.y) %>%
        unique() %>%
        ungroup() %>%
        group_by(record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations_with_metadata <- merge_metadata(ids, extra_merge_fields)

    }

    unique_citations_with_metadata <-unique_citations_with_metadata %>%
      select(-c(source1, label1, source2, label2))

    return(unique_citations_with_metadata)
  }

}


