#' Load in citations for deduplication
#'
#' This function loads in a citation file within the shiny app
#' @import RefManageR
#' @importFrom glue glue
#' @import bibliometrix
#' @import XML
#' @importFrom utils read.csv read.table
#' @param paths Relative paths to the citations file or files
#' @param method  Import method
#' @param names File names of input file or files
#' @return A dataframe of the loaded citations.
#' @export

 load_multi_search <-function(paths, names, method){

  df_list <- list()

  for (i in 1:length(paths)) {

    path <- paths[i]
    name <- names[i]

  if(method == "bib"){

    # try wos format
    suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "wos", format="bibtex"), silent=TRUE)))

     if(exists("newdat")){

    # Create a lookup table to map Field to Abbreviation
    lookup_table <- stats::setNames(field_codes_wos$Field, field_codes_wos$Abbreviation)

    # Rename the columns in df_original using the lookup_table
    colnames(newdat) <- lookup_table[colnames(newdat)]

    # Remove columns
    keep.cols <- names(newdat) %in% NA
    newdat <- newdat [! keep.cols]
    rownames(newdat) <- 1:nrow(newdat)

  }
    if(!exists("newdat")){

      # try pubmed format
      suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "pubmed", format = "pubmed"), silent=TRUE)))

       if(exists("newdat")){

      # Create a lookup table to map Field to Abbreviation
      lookup_table <- setNames(field_codes_pubmed$Field, field_codes_pubmed$Abbreviation)

      # Rename the columns in df_original using the lookup_table
      colnames(newdat) <- lookup_table[colnames(newdat)]

      # Remove columns
      keep.cols <- names(newdat) %in% NA
      newdat <- newdat [! keep.cols]
      rownames(newdat) <- 1:nrow(newdat)

    # additional formatting for issn - keeping only ISSN vs other identifiers
    newdat$isbn <- trimws(stringr::str_extract(newdat$issn, ".{4}-.{4}.(?=\\((ELECTRONIC|PRINT\\)))"))
       }
    }

    if(!exists("newdat")){

    try(newdat <- RefManageR::ReadBib(path, check =FALSE))

    }

    newdat <- as.data.frame(newdat)

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    newdat$pages <- lapply(newdat$pages, function(x) gsub("--", "-", x))

    newdat$file_name <- name
    df_list[[i]] <- newdat


    remove(newdat)

  }

  if(method == "zotero_csv"){

    newdat <- utils::read.csv(path)
    newdat <- newdat %>%
      dplyr::rename(record_id = Key,
                    year = Publication.Year,
                    journal = Publication.Title,
                    keywords = Manual.Tags )

    names(newdat) <- tolower(names(newdat))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA


    newdat$file_name <- name
    df_list[[i]] <- newdat
  }

  if(method == "ris"){

    # newdat <- synthesisr::read_refs(path)
    # Read in the RIS file
    ris_lines <- readLines(path)

    # Create a list to store each reference's data
    records <- list()
    current_record <- list()

    # Process each line in the RIS file
    for (i in seq_along(ris_lines)) {
      line <- ris_lines[i]

      # If the line starts with a field (e.g., TY for Type of Reference)
      if (grepl("^[A-Z0-9]{2,5}\\s+-\\s", line)) {

        # Extract the field and value
        field <- substr(line, 1, 2)
        value <- substr(line, 7, nchar(line))

        # Skip adding empty fields
        if (value != "") {
          if (field == "AU") {
            if ("AU" %in% names(current_record)) {
              # Append to the existing AU vector
              current_record[["AU"]] <- paste0(current_record[["AU"]], "; ", value)
            } else {
              # Initialize with the first author
              current_record[["AU"]] <- value
            }
          }
          else{
            # Add or overwrite field-value pairs in the current record
            current_record[[field]] <- value
          }
        }
      }

      else if(grepl("^\\s", line)) {
        # Handle continuation of the previous field
        value <- trimws(line)
        last_field <- field

        if (!is.null(last_field)) {
          # Append the continuation to the previous value
          current_record[[last_field]] <- paste0(trimws(current_record[[last_field]]), " ", value)
        }
      }

      # If the line is 'ER' (End of Record), save the current record and reset
      if (grepl("^ER\\s.*-", line)) {

        # Extract the value of ER field
        value <- substr(line, 7, nchar(line))

        # Only append the record if it has meaningful data
        if (value == "") {
          # If current record contains more than 2 fields (considering at least some information is present)
          if (length(current_record) > 2) {
            records <- append(records, list(current_record))
            current_record <- list()  # Reset for next record
          }
        } else {
          print("uh-oh: Unexpected non-empty 'ER' field.")
          return(NULL)
        }
      }

      if (line=="") {
        # If current record contains more than 2 fields (considering at least some information is present)
        if (length(current_record) > 2) {
          records <- append(records, list(current_record))
          current_record <- list()  # Reset for next record
        }
        else {
          current_record <- list()
        }
      }

      # Detect the end of a file
      if (i == length(ris_lines)) {
        if (length(current_record) > 2) {
          # Add the current record to the list of records
          records <- append(records, list(current_record))
          current_record <- list()
        }
      }
    }

    # Convert list of records to a data frame
    newdat <- bind_rows(records)

    # Rename columns with more meaningful names
    newdat <- newdat %>%
      dplyr::rename_with(
        .fn = ~ case_when(
          . == "AU" ~ "author",
          . == "TI" ~ "title",
          . == "UR" ~ "url",
          . == "AB" ~ "abstract",
          . == "PY" ~ "year",
          . == "DO" ~ "doi",
          . == "T2" ~ "journal",
          . == "SP" ~ "pages",
          . == "VL" ~ "volume",
          . == "IS" ~ "number",
          . == "ID" ~ "record_id",
          . == "SN" ~ "isbn",
          . == "DB" ~ "source",
          TRUE ~ .
        )
      )

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA


    cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source', 'url')
    newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

    newdat$file_name <- name
    df_list[[i]] <- newdat

    return(newdat)

  }

    if(method == "endnote"){

      newdat<- XML::xmlParse(path)
      x <-  XML::getNodeSet(newdat,'//record')

      xpath2 <-function(x, ...){
        y <- XML::xpathSApply(x, ...)
        y <- gsub(",", ";;;;;", y)  # remove commas if using comma separator
        ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
      }

      newdat <- data.frame(
        author = sapply(x, xpath2, ".//author", xmlValue),
        year   = sapply(x, xpath2, ".//dates/year", xmlValue),
        journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
        doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
        title = sapply(x, xpath2, ".//titles/title", xmlValue),
        pages = sapply(x, xpath2, ".//pages", xmlValue),
        volume = sapply(x, xpath2, ".//volume", xmlValue),
        number = sapply(x, xpath2, ".//number", xmlValue),
        abstract = sapply(x, xpath2, ".//abstract", xmlValue),
        record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
        isbn = sapply(x, xpath2, ".//isbn", xmlValue),
        secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
        accession_number = sapply(x, xpath2, ".//accession-num", xmlValue),
        keywords = sapply(x, xpath2, ".//keywords", xmlValue),
        type = sapply(x, xpath2, ".//ref-type", xmlValue),
        label = sapply(x, xpath2, ".//label", xmlValue),
        source = sapply(x, xpath2, ".//remote-database-name", xmlValue),
        url = sapply(x, xpath2, ".//urls/related-urls/url", xmlValue),
        database = sapply(x, xpath2, ".//remote-database-name", xmlValue)) %>%
        mutate(journal = ifelse(is.na(.data$journal), .data$secondary_title, .data$journal))

      cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
      newdat[cols[!(cols %in% colnames(newdat))]] = NA

      newdat <- newdat %>%
        mutate(across(everything(), ~ gsub(.x, pattern = ";;;;;", replacement = ",")))

      newdat$file_name <- name
      df_list[[i]] <- newdat
    }

  if(method == "csv"){

        cols <- c("label","isbn", "source")
        newdat <- read.csv(path)
        newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat$file_name <- name
        df_list[[i]] <- newdat
}


if(method == "txt"){

  cols <- c("label","isbn","source", "url")
  newdat <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Create a mapping from source names to your desired names
  name_map <- c(
    "Author"          = "author",
    "Year"            = "year",
    "Secondary.Title" = "journal",
    "DOI"             = "doi",
    "Title"           = "title",
    "Pages"           = "pages",
    "Volume"          = "volume",
    "Number"          = "number",
    "Abstract"        = "abstract",
    "Custom.1"        = "record_id",
    "ISBN.ISSN"       = "isbn",
    "Url"             = "url"
  )

  # Rename only the columns that match
  for (old in names(name_map)) {
    if (old %in% names(newdat)) {
      names(newdat)[names(newdat) == old] <- name_map[old]
    }
  }

  newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat$file_name <- name
        df_list[[i]] <- newdat
}
  }

  # make sure year is character in all
  for (i in 1:length(df_list)) {
    df_list[[i]]$year <- as.character(df_list[[i]]$year)
  }

  newdat <- dplyr::bind_rows(df_list)

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source', 'url')
  newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

  return(newdat)

}


 #' Load in citations for deduplication
 #'
 #' This function loads a citations file using the specified import method. If the method is not provided, it defaults to one based on the file extension.
 #' Supported methods: "endnote", "csv", "txt", "bib", "zotero_csv", "ris".
 #'
 #' @import RefManageR
 #' @import bibliometrix
 #' @importFrom utils read.csv read.table
 #' @param path File path(s) to one or more citations file(s). If more than one file is passed, the file name is added into the `source` field to distinguish where citations came from
 #' @param method Import method. Valid options are "endnote", "csv", "txt", "bib", "zotero_csv", and "ris". If not provided, the method will be inferred from the file extension.
 #' @return A dataframe of the citations.
 #' @export

 load_search <- function(path, method = NULL) {

   if (length(path) > 1) {
     # If there are multiple paths, handle recursion
     if (length(method) == length(path) | length(method) == 1 | is.null(method)) {
       if (length(method) == 1) {
         # If only one method is provided, replicate it for all paths
         method <- rep(method, length(path))
       }
       # Recursively call the function for each path and bind the results
       return(do.call(rbind, lapply(seq_along(path), function(i) {
         df <- load_search(path[i], method[i])
         df$source <- path[i]
         df
       })))
     } else {
       stop("Length of 'method' should be either 1, equal to the length of 'path', or not provided.")
     }
   }

   # Infer method from file extension if not provided
   if (is.null(method)) {
     ext <- tools::file_ext(path)
     method <- switch(ext,
                      # "xml" = "endnote",
                      "csv" = "csv",
                      "txt" = "txt",
                      "bib" = "bib",
                      "ris" = "ris",
                      stop("Method cannot be inferred from file extension. Please specify a valid `method` argument.")
     )
   }

   # Validate method
   valid_methods <- c("endnote", "csv", "txt", "bib", "zotero_csv", "ris")
   if (!method %in% valid_methods) {
     stop(glue::glue("Invalid method '{method}'. Valid options are: {paste(valid_methods, collapse = ', ')}"))
   }

  if(method == "endnote"){

    newdat<- XML::xmlParse(path)
    x <-  XML::getNodeSet(newdat,'//record')

    xpath2 <-function(x, ...){
      y <- XML::xpathSApply(x, ...)
      y <- gsub(",", ";;;;;", y)  # remove commas if using comma separator
      ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
    }

    newdat <- data.frame(
      author = sapply(x, xpath2, ".//author", xmlValue),
      year   = sapply(x, xpath2, ".//dates/year", xmlValue),
      journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
      doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
      title = sapply(x, xpath2, ".//titles/title", xmlValue),
      pages = sapply(x, xpath2, ".//pages", xmlValue),
      volume = sapply(x, xpath2, ".//volume", xmlValue),
      number = sapply(x, xpath2, ".//number", xmlValue),
      abstract = sapply(x, xpath2, ".//abstract", xmlValue),
      record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
      isbn = sapply(x, xpath2, ".//isbn", xmlValue),
      secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
      label = sapply(x, xpath2, ".//label", xmlValue),
      url = sapply(x, xpath2, ".//url", xmlValue),
      source = sapply(x, xpath2, ".//remote-database-name", xmlValue)) %>%
      mutate(journal = ifelse(is.na(.data$journal), .data$secondary_title, .data$journal))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    newdat <- newdat %>%
      mutate(across(everything(), ~ gsub(.x, pattern = ";;;;;", replacement = ",")))

  }

  if(method == "csv"){

    newdat <- utils::read.csv(path)
    names(newdat) <- tolower(names(newdat))
    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA
  }


  if(method == "txt"){

    newdat <- utils::read.table(path)
    names(newdat) <- tolower(names(newdat))
    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    return(newdat)
  }


  if(method == "bib"){


    # try wos format
    suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "wos", format="bibtex"), silent=TRUE)))

    if(exists("newdat")){

      # Create a lookup table to map Field to Abbreviation
      lookup_table <- setNames(field_codes_wos$Field, field_codes_wos$Abbreviation)

      # Rename the columns in df_original using the lookup_table
      colnames(newdat) <- lookup_table[colnames(newdat)]

      # Remove columns
      keep.cols <- names(newdat) %in% NA
      newdat <- newdat [! keep.cols]
      rownames(newdat) <- 1:nrow(newdat)

    }
    if(!exists("newdat")){

      # try pubmed format
      suppressMessages(suppressWarnings(try(newdat <- bibliometrix::convert2df(path, dbsource = "pubmed", format = "pubmed"), silent=TRUE)))

      if(exists("newdat")){

        # Create a lookup table to map Field to Abbreviation
        lookup_table <- stats::setNames(field_codes_pubmed$Field, field_codes_pubmed$Abbreviation)

        # Rename the columns in df_original using the lookup_table
        colnames(newdat) <- lookup_table[colnames(newdat)]

        # Remove columns
        keep.cols <- names(newdat) %in% NA
        newdat <- newdat [! keep.cols]
        rownames(newdat) <- 1:nrow(newdat)

        # additional formatting for issn - keeping only ISSN vs other identifiers
        newdat$isbn <- trimws(stringr::str_extract(newdat$issn, ".{4}-.{4}.(?=\\((ELECTRONIC|PRINT\\)))"))
      }
    }

    if(!exists("newdat")){

      try(newdat <- RefManageR::ReadBib(path, check =FALSE))

    }

    newdat <- as.data.frame(newdat)

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

  }


  if(method == "zotero_csv"){


    newdat <- utils::read.csv(path)
    newdat <- newdat %>%
      dplyr::rename(record_id = Key,
                    year = Publication.Year,
                    journal = Publication.Title,
                    keywords = Manual.Tags )

    names(newdat) <- tolower(names(newdat))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA
    newdat$pages <- lapply(newdat$pages, function(x) gsub("--", "-", x))

  }

  if(method == "ris"){


    # newdat <- synthesisr::read_refs(path)

    # Read in the RIS file
    ris_lines <- readLines(path)

    # Create a list to store each reference's data
    records <- list()
    current_record <- list()

    # Process each line in the RIS file
    for (i in seq_along(ris_lines)) {
      line <- ris_lines[i]

      # If the line starts with a field (e.g., TY for Type of Reference)
      if (grepl("^[A-Z0-9]{2,5}\\s+-\\s", line)) {

        # Extract the field and value
        field <- substr(line, 1, 2)
        value <- substr(line, 7, nchar(line))

        # Skip adding empty fields
        if (value != "") {
          if (field == "AU") {
            if ("AU" %in% names(current_record)) {
              # Append to the existing AU vector
              current_record[["AU"]] <- paste0(current_record[["AU"]], "; ", value)
            } else {
              # Initialize with the first author
              current_record[["AU"]] <- value
            }
          }
          else{
            # Add or overwrite field-value pairs in the current record
            current_record[[field]] <- value
          }
        }
      }

          else if(grepl("^\\s", line)) {
        # Handle continuation of the previous field
        value <- trimws(line)
        last_field <- field

        if (!is.null(last_field)) {
          # Append the continuation to the previous value
          current_record[[last_field]] <- paste0(trimws(current_record[[last_field]]), " ", value)
        }
          }

      # If the line is 'ER' (End of Record), save the current record and reset
      if (grepl("^ER\\s.*-", line)) {

        # Extract the value of ER field
        value <- substr(line, 7, nchar(line))

        # Only append the record if it has meaningful data
        if (value == "") {
          # If current record contains more than 2 fields (considering at least some information is present)
          if (length(current_record) > 2) {
            records <- append(records, list(current_record))
            current_record <- list()  # Reset for next record
          }
        } else {
          print("uh-oh: Unexpected non-empty 'ER' field.")
          return(NULL)
        }
      }

      if (line=="") {
          # If current record contains more than 2 fields (considering at least some information is present)
          if (length(current_record) > 2) {
            records <- append(records, list(current_record))
            current_record <- list()  # Reset for next record
          }
         else {
           current_record <- list()
         }
      }

      # Detect the end of a file
      if (i == length(ris_lines)) {
        if (length(current_record) > 2) {
          # Add the current record to the list of records
          records <- append(records, list(current_record))
          current_record <- list()
        }
      }
    }

    # Convert list of records to a data frame
    newdat <- bind_rows(records)

    # Rename columns with more meaningful names
    newdat <- newdat %>%
      dplyr::rename_with(
        .fn = ~ case_when(
          . == "AU" ~ "author",
          . == "TI" ~ "title",
          . == "UR" ~ "url",
          . == "AB" ~ "abstract",
          . == "PY" ~ "year",
          . == "DO" ~ "doi",
          . == "T2" ~ "journal",
          . == "SP" ~ "pages",
          . == "VL" ~ "volume",
          . == "IS" ~ "number",
          . == "ID" ~ "record_id",
          . == "SN" ~ "isbn",
          . == "DB" ~ "source",
          TRUE ~ .
        )
      )

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source", "url")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

  }

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source', 'url')
  newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  return(newdat)

}


