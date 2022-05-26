#' Load in citations for deduplication
#'
#' This function loads in an Endnote XML file OR csv OR text.
#'
#' @param path File path to the input file
#' @param method  Loading citations methoddepending on file format
#' @return A dataframe of the Endnote references
#' @export
load_search <-function(path, method){

  if(method == "endnote"){


        newdat<-xmlParse(path)
        x <-  getNodeSet(newdat,'//record')

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
          "PDF Relative Path" = sapply(x, xpath2, ".//urls/pdf-urls", xmlValue),
          url = sapply(x, xpath2, ".//urls/related-urls", xmlValue),
          label = sapply(x, xpath2, ".//label", xmlValue)
        )

        newdat <- newdat %>%
          mutate(label = ifelse(is.na(label), "NA", paste(label)))
        return(newdat)
  }

  if(method == "csv"){

        cols <- c("label","isbn")
        newdat <- read.csv(path)
        newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat <- newdat %>%
          dplyr::select(author,
                 year,
                 journal,
                 doi,
                 title,
                 pages,
                 volume,
                 number,
                 abstract,
                 record_id,
                 isbn,
                 label) %>%
          mutate(label = ifelse(is.na(label), "NA", paste(label))) %>%
          mutate(isbn = ifelse(is.na(isbn), "NA", paste(isbn)))


        return(newdat)
}


if(method == "txt"){

  cols <- c("label","isbn")
  newdat <- read.table(path)
  newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat <- newdat %>%
          dplyr::select(author,
                 year,
                 journal,
                 doi,
                 title,
                 pages,
                 volume,
                 number,
                 abstract,
                 record_id,
                 isbn,
                 label)

        newdat <- newdat %>%
          mutate(label = ifelse(is.na(label), "NA", paste(label))) %>%
          mutate(isbn = ifelse(is.na(isbn), "NA", paste(isbn)))

        return(newdat)
}
}
