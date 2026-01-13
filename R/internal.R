#' Add IDs to Citation Data
#'
#' This function adds an ID to citation data if missing.
#'
#' @param raw_citations A dataframe containing citation data with relevant columns.
#' @return A dataframe of citations with IDs.
#' @import dplyr
#' @noRd
#'
#' @examples
#' # Load example data
#' data <- data.frame(
#'   author = c("Author A", "Author B", "Author C"),
#'   title = c("Title 1", "Title 2", "Title 3"),
#'   year = c(2020, 2019, 2018)
#' )
#'
#' # Add IDs to citation data
#' data_with_ids <- add_id_citations(data)
#' head(data_with_ids)
#'
add_id_citations <- function(raw_citations) {
  # add record id from row number if missing
  raw_citations <- raw_citations %>%
    mutate(record_id = as.character(row_number() + 1000))
}

#' Order Citation Data
#'
#' This function orders citation data for deduplication.
#'
#' @param raw_citations A dataframe containing citation data with relevant columns.
#' @param extra_merge_fields Additional fields to include in the ordered citations.
#' @return A dataframe of ordered citations with an ID.
#' @import dplyr
#' @import utf8
#' @noRd
#'
#' @examples
#' # Load example data
#' data <- data.frame(
#'   author = c("Author A", "Author B", "Author C"),
#'   title = c("Title 1", "Title 2", "Title 3"),
#'   year = c(2020, 2019, 2018),
#'   journal = c("Journal A", "Journal B", "Journal C"),
#'   abstract = c("Abstract 1", "", "Abstract 3"),
#'   doi = c("doi1", "doi2", "doi3"),
#'   number = c(1, 2, 3),
#'   pages = c("1-10", "20-30", "40-50"),
#'   volume = c(10, 20, 30),
#'   isbn = c("isbn1", "isbn2", "isbn3"),
#'   record_id = c(1001, 1002, 1003),
#'   label = c("Label 1", "Label 2", "Label 3"),
#'   source = c("Source 1", "Source 2", "Source 3")
#' )
#'
#' # Order citation data
#' ordered_data <- order_citations(data, extra_merge_fields = c("field1", "field2"))
#' head(ordered_data)
#'
order_citations <- function(raw_citations, extra_merge_fields) {
  # arrange by Year and presence of an Abstract - we want to keep newer records and records with an abstract preferentially
  ordered_citations <- raw_citations %>%
    arrange(abstract, year) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8

  # select relevant columns
  ordered_citations <- ordered_citations  %>%
    dplyr::select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label, source, {{extra_merge_fields}})

  return(ordered_citations)
}

#' Format Citation Data
#'
#' This function formats citation data for deduplication.
#'
#' @param raw_citations A dataframe containing citation data with relevant columns.
#' @return A dataframe of formatted citations with an ID.
#' @import dplyr
#' @import utf8
#' @noRd
#'
#' @examples
#' # Load example data
#' data <- data.frame(
#'   author = c("Author A", "Author B", "Author C"),
#'   title = c("Title 1", "Title 2", "Title 3"),
#'   year = c(2020, 2019, 2018),
#'   journal = c("Journal A", "Journal B", "Journal C"),
#'   abstract = c("Abstract 1", "", "Abstract 3"),
#'   doi = c("doi1", "doi2", "doi3"),
#'   number = c(1, 2, 3),
#'   pages = c("1-10", "20-30", "40-50"),
#'   volume = c(10, 20, 30),
#'   isbn = c("isbn1", "isbn2", "isbn3"),
#'   record_id = c(1001, 1002, 1003),
#'   label = c("Label 1", "Label 2", "Label 3"),
#'   source = c("Source 1", "Source 2", "Source 3")
#' )
#'
#' # Format citation data
#' formatted_data <- format_citations(data)
#' head(formatted_data)
#'
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

#' Match Citations
#'
#' This function identifies matching pairs of citations.
#'
#' @param formatted_citations A dataframe containing formatted citation data with relevant columns and an ID column.
#' @return A dataframe of citation pairs.
#' @import RecordLinkage
#' @import parallel
#' @import parallelly
#' @noRd
#'
#' @examples
#' # Load example data
#' data <- data.frame(
#'   author = c("Author A", "Author B", "Author C"),
#'   title = c("Title 1", "Title 2", "Title 3"),
#'   year = c(2020, 2019, 2018),
#'   journal = c("Journal A", "Journal B", "Journal C"),
#'   abstract = c("Abstract 1", "", "Abstract 3"),
#'   doi = c("doi1", "doi2", "doi3"),
#'   number = c(1, 2, 3),
#'   pages = c("1-10", "20-30", "40-50"),
#'   volume = c(10, 20, 30),
#'   isbn = c("isbn1", "isbn2", "isbn3"),
#'   record_id = c(1001, 1002, 1003),
#'   label = c("Label 1", "Label 2", "Label 3"),
#'   source = c("Source 1", "Source 2", "Source 3")
#' )
#'
#' # Match citation pairs
#' matched_pairs <- match_citations(data)
#' head(matched_pairs)
#'
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
    suppressWarnings(try(pairs$year <- mapply(jarowinkler, pairs$year1, pairs$year2), silent = TRUE))
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
    filter(title_match_ls>0.80 & author>0.75 |
             title_match_ls>0.80 & abstract>0.80 |
             title>0.85 & abstract>0.90 |
             title>0.85 & pages>0.90 |
             title>0.85 & author>0.90 |
             title_match_ls>0.80 & isbn>0.99 |
             doi>0.999 |
             title_match_ls>0.80 & journal>0.80) %>%
    filter(!(as.numeric(year1) - as.numeric(year2) >2) | doi>0.999) %>%
    filter(!(as.numeric(year2) - as.numeric(year1) >2) | doi>0.999)
    select(-title_match_ls) 
  
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
#' @param raw_citations formatted citation data
#' @param post_auto_dedup has this data already been through deduplication? (will it have a duplicate id column?)
#' @return Dataframe of formatted citation data with duplicate id
#' @import dplyr
#' @noRd
generate_dup_id <- function(true_pairs, raw_citations, keep_source, keep_label, post_auto_dedup = FALSE){

  if(post_auto_dedup == TRUE){

  # get df of duplicate ids and record ids
  true_pairs_small <- true_pairs %>%
    select(duplicate_id.x, duplicate_id.y) %>%
    unique()

  # Create a graph from the Edges1 DataFrame
  g <- igraph::graph_from_data_frame(true_pairs_small, directed = FALSE)

  # Get the connected components of the graph
  cc <- igraph::components(g)

  # Add a new column to the Edges1 DataFrame with the component ID for each row
  true_pairs_small$ComponentID <- cc$membership[match(true_pairs_small$duplicate_id.x, names(cc$membership))]

  # Get the unique component IDs
  uniqueIDs <- unique(true_pairs_small$ComponentID)

  # make character
  duplicate_id <- true_pairs_small %>%
    group_by(ComponentID) %>%
    tidyr::unite(record_ids, duplicate_id.x, duplicate_id.y, sep = ", ") %>%
    summarise(record_id = paste(record_ids, collapse = ", ")) %>%
    tidyr::separate_rows(record_id, sep= ", ") %>%
    distinct()

  duplicate_id$record_id <- as.character(duplicate_id$record_id)
  raw_citations$record_id <- as.character(raw_citations$record_id)

  duplicate_id <- duplicate_id %>%
    right_join(raw_citations) %>%
    mutate(ComponentID = ifelse(is.na(ComponentID), paste0(max(duplicate_id$ComponentID)+row_number()), ComponentID))
  duplicate_id <- unique(duplicate_id)

  } else {

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

    duplicate_id$record_id <- as.character(duplicate_id$record_id)
    raw_citations$record_id <- as.character(raw_citations$record_id)

    duplicate_id <- duplicate_id %>%
      right_join(raw_citations) %>%
      mutate(ComponentID = ifelse(is.na(ComponentID), paste0(max(duplicate_id$ComponentID)+row_number()), ComponentID))

  }

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
      group_by(ComponentID) %>%
      arrange(record_id) %>%
      mutate(duplicate_id = first(.data$record_id)) %>%
      ungroup() %>%
      select(-ComponentID)
  }

  return(citations_duplicate_id)

}
####------ Process for manual dedup ------ ####
process_possible_pairs <- function(maybe_pairs, ordered_citations, matched_pairs_with_ids, extra_merge_fields){

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

    return(maybe_pairs)
  }
}

####------ Adding missing columns ------ ####
add_missing_cols <- function(raw_citations){
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

  return(raw_citations)
}

####------ Remove double values within string ------ ####
# Function to remove duplicates within a string
remove_string_dups <- function(string) {
  elements <- unlist(strsplit(string, ", "))  # Split the string into individual elements
  unique_elements <- unique(elements)         # Remove duplicates
  cleaned_string <- paste(unique_elements, collapse = ", ")  # Combine elements back into a string
  return(cleaned_string)
}


### --- Format datafarme if rerunning dedup --- ####
format_rerun <- function(raw_citations){

  if("duplicate_id" %in% names(raw_citations)){

    message("Re-running ASySD on dataset...")
    raw_citations <- raw_citations %>%
      rename(record_id = duplicate_id)
    return(raw_citations)

  }  else {
    return(raw_citations)
  }
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
#' @importFrom stats na.omit
#' @import dplyr

merge_metadata <- function(matched_pairs_with_ids, extra_merge_fields) {

    # If on a rerun, include record_ids into the merging, otherwise merge record_id field
    if (!"record_ids" %in% names(matched_pairs_with_ids)) {
      matched_pairs_with_ids$record_ids <- matched_pairs_with_ids$record_id
    }

    merge_fields <- c("record_ids", "label", "source", extra_merge_fields)

    paste_unless_blank_or_na <- function(x) {
      if (all(is.na(x))) return(NA)
      if (all(x == "")) return ("")
      paste(na.omit(x), collapse = ';;;')
    }

      all_metadata_with_duplicate_id <- matched_pairs_with_ids %>%
        select(-record_id) %>%
        mutate_if(is.character, utf8::utf8_encode) %>% # ensure all utf8
        mutate_all(~replace(., .=='NA', NA)) %>% #replace NA
        group_by(.data$duplicate_id) %>%
        summarise(across(everything(), ~ trimws(paste_unless_blank_or_na(.x))), .groups = "drop") %>% #merge all rows with same dup id, dont merge NA values
        mutate(across(c(everything(), -{{merge_fields}}), ~ gsub(.x, pattern = ";;;.*", replacement = ""))) %>% #remove extra values in each col, keep first one only
        mutate(across({{merge_fields}}, ~ gsub(.x, pattern = ";;;", replacement = ", ")))

      # Ensure IDs are not repeated within the field (not sure why they would be though?)
      all_metadata_with_duplicate_id$record_ids <- sapply(all_metadata_with_duplicate_id$record_ids, remove_string_dups)

      return(all_metadata_with_duplicate_id)

}
