#' Load in Endnote references via XML file
#'
#' This function loads in an Endnote XML file. It assumes that thefile is exported in
#' standard Endnote XML format.
#'
#' @param path Path to the input file
#' @return A dataframe of the Endnote references
#' @export
load_refs <-function(path, method){

  if(method == "endnote"){


        newdat<-xmlParse(path)
        x <-  getNodeSet(newdat,'//record')

        xpath2 <-function(x, ...){
          y <- xpathSApply(x, ...)
          y <- gsub(",", "", y)  # remove commas if using comma separator
          ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
        }

        newdat <- data.frame(
          Author = sapply(x, xpath2, ".//contributors/authors", xmlValue),
          Year   = sapply(x, xpath2, ".//dates/year", xmlValue),
          Journal = sapply(x, xpath2, ".//periodical/full-title", xmlValue),
          DOI = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
          Title = sapply(x, xpath2, ".//titles/title", xmlValue),
          Pages = sapply(x, xpath2, ".//pages", xmlValue),
          Volume = sapply(x, xpath2, ".//volume", xmlValue),
          Number = sapply(x, xpath2, ".//number", xmlValue),
          Abstract = sapply(x, xpath2, ".//abstract", xmlValue),
          RecordID = sapply(x, xpath2, ".//rec-number", xmlValue),
          ISBN = sapply(x, xpath2, ".//isbn", xmlValue),
          SecondaryTitle = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
          "PDF Relative Path" = sapply(x, xpath2, ".//urls/pdf-urls", xmlValue),
          Url = sapply(x, xpath2, ".//urls/related-urls", xmlValue),
          Label = sapply(x, xpath2, ".//label", xmlValue)
        )

        newdat <- newdat %>%
          mutate(Label = ifelse(is.na(Label), "NA", paste(Label)))
        return(newdat)
  }

  if(method == "csv"){

        cols <- c("Label","ISBN")
        newdat <- read.csv(path)
        newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat <- newdat %>%
          select(Author,
                 Year,
                 Journal,
                 DOI,
                 Title,
                 Pages,
                 Volume,
                 Number,
                 Abstract,
                 RecordID,
                 ISBN,
                 Label) %>%
          mutate(Label = ifelse(is.na(Label), "NA", paste(Label))) %>%
          mutate(ISBN = ifelse(is.na(ISBN), "NA", paste(ISBN)))


        return(newdat)
}


if(method == "txt"){

  cols <- c("Label","ISBN")
  newdat <- read.table(path)
  newdat[cols[!(cols %in% colnames(newdat))]] = NA

        newdat <- newdat %>%
          select(Author,
                 Year,
                 Journal,
                 DOI,
                 Title,
                 Pages,
                 Volume,
                 Number,
                 Abstract,
                 RecordID,
                 ISBN,
                 Label)

        newdat <- newdat %>%
          mutate(Label = ifelse(is.na(Label), "NA", paste(Label))) %>%
          mutate(ISBN = ifelse(is.na(ISBN), "NA", paste(ISBN)))

        return(newdat)
}
}
