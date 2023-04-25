utils::globalVariables(c("Abstract", "AlternateName", "Author", "AuthorAddress", "Authors", "Custom 1",
                         "CustomId", "DOI", "ISBN/ISSN", "Keywords", "Label", "Name of Database",
                         "Number", "Pages", "PublicationName", "Reference Type",
                         "ReferenceType", "Secondary Title", "Title", "Volume", "Year",
                         "record_ids"))

#' Load in citations for deduplication
#'
#' This function loads in a citation file within the shiny app
#'
#' @param path Relative path to the citations file
#' @param method  Import method
#' @param name File name of input file or files
#' @return A dataframe of the citations
#' @import XML
#' @import RefManageR

load_multi_search <-function(paths, names, method){

  df_list <- list()

  for (i in 1:length(paths)) {

    path <- paths[i]
    name <- names[i]

  if(method == "bib"){

    newdat <- RefManageR::ReadBib(path, check =FALSE)
    newdat <- as.data.frame(newdat)

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA


    newdat$file_name <- name
    df_list[[i]] <- newdat
  }

  if(method == "zotero_csv"){

    newdat <- read.csv(path)
    newdat <- newdat %>%
      dplyr::rename(record_id = Key,
                    year = Publication.Year,
                    journal = Publication.Title,
                    keywords = Manual.Tags )

    names(newdat) <- tolower(names(newdat))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    newdat$file_name <- name
    df_list[[i]] <- newdat
  }

  if(method == "ris"){

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")

    newdat <- synthesisr::read_refs(path)
    if ("booktitle" %in% colnames(newdat)) {
      newdat <-newdat %>%
        tidyr::unite(title, title, booktitle, na.rm = TRUE)
    }

    if ("start_page" %in% colnames(newdat) &
        "pages" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, pages, start_page, end_page, sep="-", na.rm=TRUE) %>%
        select(-start_page, -end_page)
    }

     if ("start_page" %in% colnames(newdat) &
         "end_page" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
    }

    if (!"journal" %in% colnames(newdat)){
      newdat <- newdat %>%
        rename(journal = source)
    }

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    newdat$file_name <- name
    df_list[[i]] <- newdat
  }

  if(method == "endnote"){


        newdat<- XML::xmlParse(path)
        x <-  XML::getNodeSet(newdat,'//record')

        xpath2 <-function(x, ...){
          y <- xpathSApply(x, ...)
          y <- gsub(",", "", y)  # remove commas if using comma separator
          ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
        }

        newdat <- data.frame(
          author = sapply(x, xpath2, ".//contributors/authors", xmlValue),
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
          database = sapply(x, xpath2, ".//remote-database-name", xmlValue)) %>%
          mutate(journal = ifelse(is.na(journal), .data$secondary_title, journal))

        cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
        newdat[cols[!(cols %in% colnames(newdat))]] = NA

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

  cols <- c("label","isbn","source")
  newdat <- read.table(path)
  newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat$file_name <- name
        df_list[[i]] <- newdat
}
  }

  newdat <- dplyr::bind_rows(df_list)

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

  return(newdat)

}


#' Load in citations for deduplication
#'
#' This function loads in an citations file.
#'
#' @param path File path to the citations file
#' @param method  Import method
#' @return A dataframe of the citations
#' @export
#' @import XML
#' @import RefManageR
#' @import utils
load_search <-function(path, method){

  if(method == "endnote"){


    newdat<- XML::xmlParse(path)
    x <-  XML::getNodeSet(newdat,'//record')

    xpath2 <-function(x, ...){
      y <- XML::xpathSApply(x, ...)
      y <- gsub(",", "", y)  # remove commas if using comma separator
      ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
    }

    newdat <- data.frame(
      author = sapply(x, xpath2, ".//contributors/authors", xmlValue),
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
      source = sapply(x, xpath2, ".//remote-database-name", xmlValue)) %>%
      mutate(journal = ifelse(is.na(journal), .data$secondary_title, journal))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA
  }

  if(method == "csv"){

    cols <- c("label","isbn", "source")
    newdat <- read.csv(path)
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA
  }


  if(method == "txt"){

    cols <- c("label","isbn","source")
    newdat <- read.table(path)
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

    return(newdat)
  }


  if(method == "bib"){

    newdat <- RefManageR::ReadBib(path, check =FALSE)
    newdat <- as.data.frame(newdat)

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA


  }

  if(method == "zotero_csv"){


    newdat <- read.csv(path)
    newdat <- newdat %>%
      dplyr::rename(record_id = Key,
                    year = Publication.Year,
                    journal = Publication.Title,
                    keywords = Manual.Tags )

    names(newdat) <- tolower(names(newdat))

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA
  }

  if(method == "ris"){

    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")

    newdat <- synthesisr::read_refs(path)
    if ("booktitle" %in% colnames(newdat)) {
      newdat <-newdat %>%
        tidyr::unite(title, title, booktitle, na.rm = TRUE)
    }

    if ("start_page" %in% colnames(newdat) &
        "pages" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, pages, start_page, end_page, sep="-", na.rm=TRUE) %>%
        select(-start_page, -end_page)
    }

    if ("start_page" %in% colnames(newdat) &
        "end_page" %in% colnames(newdat)) {
      newdat <- newdat %>%
        tidyr::unite(pages, start_page, end_page, sep="-", na.rm=TRUE)
    }

    if (!"journal" %in% colnames(newdat)){
      newdat <- newdat %>%
        rename(journal = source)
    }
    cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source")
    newdat[cols[!(cols %in% colnames(newdat))]] = NA

  }

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  newdat[cols_to_modify] <- lapply(newdat[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  return(newdat)

}
