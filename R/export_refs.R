#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @import synthesisr
#' @importFrom utils read.csv read.table
#' @importFrom rlang .data
#' @param citations A data frame containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export

write_citations <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  citations$record_id <- citations$duplicate_id
  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  citations[cols_to_modify] <- lapply(citations[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

  if(type == "txt"){

    refs <- citations %>%
      dplyr::mutate("Reference Type" = "Journal Article") %>%
      dplyr::mutate("ISBN/ISSN" =  isbn,
                    URL = ifelse("url" %in% names(.), url, "")) %>%
      dplyr::rename(`Custom 1` = duplicate_id,
             Author = author,
             Title = title,
             Volume = volume,
             Number = number,
             Label = label,
             Year = year,
             Abstract = abstract,
             Pages = pages,
             DOI = doi,
             `Name of Database` = source,
             `Secondary Title` = journal) %>%
      dplyr::mutate(`Reference Type`, Author, Year,
             `Secondary Title`, DOI, Title,
              Pages, Volume, Number, Abstract,
             `Custom 1`, `ISBN/ISSN`, Label, `Name of Database`, URL)

    write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "syrf_csv"){

    refs <- citations %>%
      dplyr::rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      dplyr::mutate(AuthorAddress = "",
             Url = ifelse("url" %in% names(.), url, ""),
             AlternateName = "",
             ReferenceType = "",
             CustomId = .data$duplicate_id,
             Keywords = "",
             PdfRelativePath = paste0(duplicate_id, ".pdf")) %>%
      dplyr::select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId)

    utils::write.csv(refs, filename, row.names = F, quote=TRUE, na="")

  } else if(type == "ris"){

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    citations$source_type <- "JOUR" #for RIS import to work
    citations <- citations %>% dplyr::select(source_type , everything()) %>%
      dplyr::rename(issue = number) %>%
      select(-isbn)
    citations <- tidyr::separate(citations, pages, into = c("start_page", "end_page"), sep = "-", convert = TRUE)
    synthesisr::write_refs(citations,
                           format = "ris",
                           file = filename
    )
  } else if(type == "csv"){

    write.csv(citations, filename, row.names = F, quote=TRUE, na="")

  } else if(type == "bib"){

   citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = filename)
  }
}

#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils read.csv read.table
#' @import synthesisr
#' @param citations A dataframe containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export
#' @examples
#'
#' # Create sample citations dataframe
#' citations <- data.frame(
#'   author = c("Author1", "Author2"),
#'   year = c(2000, 2001),
#'   title = c("Title 1", "Title 2"),
#'   journal = c("Journal A", "Journal B"),
#'   doi = c("doi1", "doi2"),
#'   pages = c("1-10", "20-30"),
#'   volume = c("Vol 1", "Vol 2"),
#'   number = c("Issue 1", "Issue 2"),
#'   abstract = c("Abstract 1", "Abstract 2"),
#'   isbn = c("123456789", "987654321"),
#'   file_name = c("", ""),
#'   record_id = c(1, 2),
#'   duplicate_id = c(1, 2),
#'   label = c("Label A", "Label B"),
#'   source = c("Source A", "Source B"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example usage for exporting to txt format
#' write_citations_app(citations, type = "txt", filename = "citations.txt")
#' unlink("citations.txt")
#'
write_citations_app <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  citations[cols_to_modify] <- lapply(citations[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))


  citations <- citations %>%
    dplyr::select(-file_name)

  if(type == "txt"){


    citations$`Custom 2` <- ""

    if("flag" %in% colnames(citations))
    {
      citations$`Custom 2` <- citations$flag
    }


    refs <- citations %>%
      dplyr::mutate(`Reference Type` = "Journal Article") %>%
      dplyr::mutate(`ISBN/ISSN` = isbn,
                    URL = ifelse("url" %in% names(.), url, "")) %>%
      dplyr::rename(`Custom 1` = duplicate_id,
             Author = author,
             Title = title,
             Volume = volume,
             Number = number,
             Label = label,
             Year = year,
             Abstract = abstract,
             Pages = pages,
             DOI = doi,
             `Name of Database` = source,
             `Secondary Title` = journal) %>%
      dplyr::select("Reference Type", Author, Year,
             "Secondary Title", DOI, Title,
             Pages, Volume, Number, Abstract,
             "Custom 1", "Custom 2", "ISBN/ISSN", Label, "Name of Database", URL)

    utils::write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")


  } else if(type == "syrf_csv"){

    refs <- citations %>%
      dplyr::rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      dplyr::mutate(AuthorAddress = "",
             URL = ifelse("url" %in% names(.), url, ""),
             AlternateName = "",
             ReferenceType = "",
             CustomId = .data$duplicate_id,
             Keywords = "",
             PdfRelativePath = paste0(duplicate_id, ".pdf")) %>%
      dplyr::select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId)

    utils::write.csv(refs, filename, row.names = F, quote=TRUE, na="")

  }
  else if(type == "csv"){

    utils::write.csv(citations, filename, row.names = F, quote=TRUE, na="")

  }

  else if(type == "ris"){

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    if("flag" %in% colnames(citations))
    {
      citations$notes <- citations$flag
    }

    write_ris_df(citations, file = filename)


  } else if(type == "bib"){

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    if("flag" %in% colnames(citations))
    {
      citations$notes <- citations$flag
    }

    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = filename)
  }
}


#' Export a Data Frame to RIS Format
#'
#' @description
#' `write_ris_df()` exports a data frame containing bibliographic information
#' to a `.ris` file suitable for EndNote, Zotero, or other reference managers.
#' This function properly handles multiple authors, keywords, URLs, ISBNs, and
#' accession numbers. Missing values are automatically converted to empty strings.
#'
#' @param df A data frame containing bibliographic information. Standard columns include:
#'   * `author` — Authors separated by `;` (e.g., `"Smith, J.; Doe, A."`)
#'   * `title` — Article or book title
#'   * `year` — Publication year
#'   * `journal` — Journal name
#'   * `volume` — Volume
#'   * `number` — Issue number
#'   * `pages` — Page range
#'   * `doi` — DOI string
#'   * `abstract` — Abstract text
#'   * `keywords` — Keywords separated by `;`
#'   * `url` — URL or link to article
#'   * `isbn` — ISBN number
#'   * `accession_number` — Accession number
#'   * `type` — Reference type (e.g., `"Journal Article"`)
#'   * `label` — Optional label
#'   * `source` — Database source
#'   * `database` — Database name
#' @param file Character. Path to the `.ris` file to write.
#'
#' @details
#' The function ensures that all `NA` values in the data frame are converted
#' to empty strings. Multiple authors and keywords are separated using the
#' specified separators. The resulting RIS file can be imported into EndNote,
#' Zotero, Mendeley, or other reference managers that support RIS.
#'
#' @return Invisibly returns the RIS character vector that was written to file.
#' The primary effect is writing the RIS file.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   author = c("Smith, J.; Doe, A.", "Brown, B."),
#'   title = c("Example Paper 1", "Example Paper 2"),
#'   year = c(2020, 2021),
#'   journal = c("Journal A", "Journal B"),
#'   type = c("Journal Article", "Journal Article"),
#'   keywords = c("AI; LLM", "Machine Learning; NLP"),
#'   doi = c("10.1234/example1", "10.1234/example2"),
#'   stringsAsFactors = FALSE
#' )
#' write_ris_df(df, file = "example.ris")
#' }

write_ris_df <- function(df, file) {

  stopifnot(is.data.frame(df))

  # convert all NA to empty string
  df[is.na(df)] <- ""

  # helper: split author string safely
  split_authors <- function(x) {
    if (x == "") return(character(0))

    if (grepl("\\.\\s*,", x)) {
      authors <- unlist(strsplit(x, "\\.\\s*,\\s*"))
      authors <- paste0(trimws(authors), ".")
    } else if (grepl(";", x)) {
      authors <- trimws(unlist(strsplit(x, ";")))
    } else {
      authors <- trimws(x)
    }

    authors
  }

  # helper: split keywords
  split_keywords <- function(x) {
    if (x == "") return(character(0))
    if (grepl(";", x)) {
      kw <- trimws(unlist(strsplit(x, ";")))
    } else if (grepl(",", x)) {
      kw <- trimws(unlist(strsplit(x, ",")))
    } else {
      kw <- x
    }
    kw
  }

  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))

  for (i in seq_len(nrow(df))) {

    cat("TY  - JOUR\n", file = con)

    ## Authors
    if ("author" %in% names(df)) {
      for (a in split_authors(df$author[i])) {
        cat("AU  - ", a, "\n", sep = "", file = con)
      }
    }

    ## Core fields
    if ("title" %in% names(df) && df$title[i] != "")
      cat("TI  - ", df$title[i], "\n", sep = "", file = con)

    if ("journal" %in% names(df) && df$journal[i] != "")
      cat("JO  - ", df$journal[i], "\n", sep = "", file = con)

    if ("year" %in% names(df) && df$year[i] != "")
      cat("PY  - ", df$year[i], "\n", sep = "", file = con)

    if ("volume" %in% names(df) && df$volume[i] != "")
      cat("VL  - ", df$volume[i], "\n", sep = "", file = con)

    if ("number" %in% names(df) && df$number[i] != "")
      cat("IS  - ", df$number[i], "\n", sep = "", file = con)

    if ("pages" %in% names(df) && df$pages[i] != "") {
      pg <- strsplit(df$pages[i], "-", fixed = TRUE)[[1]]
      cat("SP  - ", trimws(pg[1]), "\n", sep = "", file = con)
      if (length(pg) > 1)
        cat("EP  - ", trimws(pg[2]), "\n", sep = "", file = con)
    }

    if ("doi" %in% names(df) && df$doi[i] != "")
      cat("DO  - ", df$doi[i], "\n", sep = "", file = con)

    if ("abstract" %in% names(df) && df$abstract[i] != "")
      cat("AB  - ", df$abstract[i], "\n", sep = "", file = con)

    ## Optional fields
    if ("notes" %in% names(df) && df$notes[i] != "")
      cat("N1  - ", df$notes[i], "\n", sep = "", file = con)

    if ("database" %in% names(df) && df$database[i] != "")
      cat("DB  - ", df$database[i], "\n", sep = "", file = con)

    if ("keywords" %in% names(df) && df$keywords[i] != "") {
      for (kw in split_keywords(df$keywords[i])) {
        cat("KW  - ", kw, "\n", sep = "", file = con)
      }
    }

    if ("url" %in% names(df) && df$url[i] != "")
      cat("UR  - ", df$url[i], "\n", sep = "", file = con)

    if ("isbn" %in% names(df) && df$isbn[i] != "")
      cat("SN  - ", df$isbn[i], "\n", sep = "", file = con)

    if ("accession_number" %in% names(df) && df$accession_number[i] != "")
      cat("AN  - ", df$accession_number[i], "\n", sep = "", file = con)

    cat("ER  -\n\n", file = con)
  }

  invisible(TRUE)
}
