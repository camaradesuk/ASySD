# Load packages ----
require(shiny)
require(readr)
require(DT)
library(shinyhelper)
require(stringr)
require(dplyr)
library(shinyalert)
library(progressr)
library(networkD3)
library(rsconnect)
library(RCurl)
library(shiny)
library(ASySD)
library(shinythemes)
library(knitr)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(bslib)

options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)

# UI ----
ui <- navbarPage(

  id = "pages",

  shinyjs::useShinyjs(),

  tags$head(includeHTML(("google-analytics.html"))),

  title="ASySD",

  theme = bs_theme(bg = "rgb(251, 251, 251)",
                   secondary = "#754E9B",
                   success = "#8894B6",
                   info = "#82D173",
                   warning = "#FFC07F",
                   danger = "#C1666B",
                   font_scale = NULL,
                   bootswatch = "flatly",
                   fg = "#000"),



  # UI side panel ----

  tabPanel("Getting started",

           uiOutput("instructions")
  ),

  tabPanel("Upload",

           sidebarLayout(

             sidebarPanel(width=3,


               # Input: select an input type  ----
               h4("Upload citations file ", icon("upload")),

               shinyWidgets::prettyRadioButtons(
                 inputId = "fileType",
                 label = "Choose a file type to upload",
                 inline = TRUE,
                 choices = c("Endnote XML",
                             "CSV", "BIB", "RIS", "Zotero CSV", "Tab delimited"),
                 status="primary") %>%
                 helper(type = "markdown", content = "uploading_studies", size="l"),

               br(),

               # Input: select a file to upload----
               fileInput("uploadfile", "Choose file(s) to upload",
                         multiple = TRUE,
                         placeholder = "No file selected")
             ),

             # UI main panel -----
             mainPanel(

               tabsetPanel(
                 tabPanel("Preview citations",

               br(),
               textOutput("Post_upload_text"),
               br(),

               # Output: datatable of citations uploaded ----
               DT::dataTableOutput("citation_table") %>% withSpinner(color="#754E9B", type=7)
           ),

             tabPanel("Check metadata",

                      DT::dataTableOutput("missing_data_table") %>% withSpinner(color="#754E9B", type=7)),

           tabPanel("Add labels/sources",

              br(),
              p("If you have uploaded citation files separately - for example an old search and a new
                search, you can edit labels and sources here if they are not already specified in the file. Note that this
                will give every citation within an upload the"), p(strong("same label or source.")),
              p("If there are multiple sources / labels within a file, below it will indicate this. We advise leaving these if you have tagged these in a reference manager before uploading, as it is not possible to change
                the source/labels of individual citations within a single file.
                To edit, double click on the relevant cell in the table below. You can preview the citations again to check that the intended change has been applied."),
              br(),
              DT::dataTableOutput("summary_table") %>% withSpinner(color="#754E9B", type=7)
           ))))),


  # UI auto deduplication page  ----
  tabPanel("Deduplicate",

           tabsetPanel(
             id = "dedup_pages",
             tabPanel("Automated deduplication",

                      sidebarLayout(

                        sidebarPanel(

                          h4("Auto-deduplication options ", icon("cog")),

                          # Text about ASySD ----
                          textOutput("ASySD_pretext"),

                          h5("Unique ID"),

                          # User option: select an ID column ----
                          uiOutput("Id_col_picker"),

                          h5("Citation to keep from each duplicate set"),

                          #  User option: how to determine which citation to keep ----
                          uiOutput("Keep_citation_picker"),

                          # Conditional user option: Select references to keep in dataset ----
                          uiOutput("keepLabel"),

                          # Conditional user option: Select references to keep in dataset ----
                          uiOutput("keepSource"),


                          # User input: auto deduplicate button -----
                          shinyWidgets::actionBttn(
                            inputId = "dedupbutton",
                            label = "Remove duplicates",
                            style = "pill",
                            color = "primary"
                          ) %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B")
                        ),

                        mainPanel(

                          h4("Auto-deduplication results"),

                          # Output: text displaying dedup results ----
                          htmlOutput("ASySD_results") %>% withSpinner(color="#754E9B", type=7)
                        )
                      )),

             tabPanel("Manual deduplication",
                      br(),

                      textOutput("Manual_pretext"),

                      br(),

                      # Button

                     fluidRow(column(width=2,

                      h4("Duplicate pair selection") %>%
                        helper(type = "markdown", content = "manual_dedup", size="l"))),

                      tags$style(HTML('.bttn-unite.bttn-primary {
                      border-color: white;
                      color: white;
                      .bttn-unite.bttn-primary:after {
                      background: #5d3d61 !important;
                      }
                      .bttn-unite.bttn-primary:before {
                      background: #6c4771 !important;
                      }')),

                      br(),

                        shinyWidgets::actionBttn(
                        inputId = "manualdedupsubmit",
                        label = "Remove duplicates",
                        style = "pill",
                        size = "sm",
                        icon = icon("reply"),
                        color = "primary")
                       %>% htmltools::tagAppendAttributes(style =  "background-color: #754E9B"),

                      # Button
                      shinyWidgets::actionBttn(
                        inputId = "manualdedupflag",
                        label = "Flag selected pairs",
                        style = "pill",
                        size = "sm",
                        color = "primary",
                        icon = icon("flag"))
             %>% htmltools::tagAppendAttributes(style =  "background-color: #8894B6"),

                      shinyWidgets::actionBttn(
                        inputId = "manualdedupflag_all",
                        label = "Flag all pairs",
                        style = "pill",
                        size = "sm",
                        color = "primary",
                        icon = icon("flag")) %>%
             htmltools::tagAppendAttributes(style =  "background-color: #8894B6"),

             br(),
             br(),

             htmlOutput("Manual_results") %>% withSpinner(color="#754E9B", type=7),

             br(),
             tags$style(HTML(".table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: #754E9B!important;}")),

             DTOutput("manual_dedup_dt")))),

  tabPanel("Summary",

           h4("Summary of deduplication steps"),

           sankeyNetworkOutput("sankey") %>% withSpinner(color="#754E9B", type=7)
  ),

  tabPanel("Download",

           sidebarLayout(

             sidebarPanel(

           h4("Download unique citations", icon("download")),

           shinyWidgets::prettyRadioButtons(
             inputId = "export_type",
             label = "Choose an export type",
             choiceNames = c("Unique citations", "All citations"),
             choiceValues = c("unique_only_export", "all_citations_export"),
             status="success"),

           shinyWidgets::prettyRadioButtons(
             inputId = "export_format",
             label = "Choose an export format",
             choiceNames = c("Endnote tab delimited", "RIS", "SyRF CSV", "CSV"),
             choiceValues = c("txt", "ris", "syrf_csv", "csv"),
             status="success"),

           materialSwitch("show_advanced", "Show advanced options",
                          value = FALSE),


           conditionalPanel(
             condition = "input.show_advanced == true",

           h5("Customise output - labels"),

           uiOutput("filterLabel"),
           materialSwitch(
             inputId = "filterLabelOnly",
             label = "Only retain citations which are unique to this label",

             status = "success"
           ),

           h5("Customise output - sources"),
           uiOutput("filterSource"),
           materialSwitch(
             inputId = "filterSourceOnly",
             label = "Only retain citations which are unique to this source",
             value = FALSE,
             status = "success"
           )),

           downloadBttn(
             "download",
             label = "Download citations",
             style = "unite",
             color = "success",
             size = "sm"
           )),

           mainPanel(

           uiOutput("ASySD_download_type"),
           br(),
           uiOutput("ASySD_pre_download"),
           br(),
           HTML(paste("<b>", "Advanced options: filtering by labels and sources", "</b>",
                      "<br>", "You may choose to only download citations with a specific label - for example
           to indicate citations obtained from a more recent systematic search. You can use the filters on the
           right to do this. If a citation is present across several labels e.g. in the old search and the new search,
           you may want to only keep the citations which are UNIQUE to the new search i.e. the NEW citations to add to your review.
           To do this, switch on the option for 'Only retain citations which are unique to this label'. This works for sources too.", "<br>"))
  ))),

  tabPanel("About",

                    uiOutput("about")
           ))

# addResourcePath("tmpuser", getwd())

server <- function(input, output, session){

  observe_helpers()

  output$instructions <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "instructions.html",
      width = "100%",
      height = 800
    )
  })

  output$about <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "about.html",
      width = "100%",
      height = 800
    )
  })
  # Reactive values from upload ----
  rv <- shiny::reactiveValues()
  rv$refdata <- NULL
  rv$citation_summary <- data.frame()

    shiny::observeEvent(input$uploadfile,{
      shiny::validate(need(input$uploadfile != "", "Select your citation file to upload..."))

      if (is.null(input$uploadfile)) {
        return(NULL)
      } else {

    input$uploadfile

    isolate(

      if(input$fileType=="Endnote XML" & all(grepl(".xml$", input$uploadfile$name))){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method="endnote")

      }  else if(input$fileType == "CSV" & all(grepl(".csv$", input$uploadfile$name))){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method="csv")

      }  else if(input$fileType == "Zotero CSV" & all(grepl(".csv$", input$uploadfile$name))){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath,  input$uploadfile$name, method="zotero_csv")

      }
      else if(input$fileType == "RIS" & all(grepl(".ris$", input$uploadfile$name))){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method="ris")
      }

      else if(input$fileType == "BIB" & all(grepl(".bib$", input$uploadfile$name))){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method="bib")

      }

      else if(input$fileType == "Tab delimited"){

        citations <- ASySD::load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method="txt")
      }

      else{
        shinyalert("Wrong file type selected",
        "The file extension doesn't match the selected upload format", type = "warning")
        shinyjs::reset(id = "uploadfile", asis = FALSE)
        return()
        }
    )

    if(length(unique(citations$record_id)) != nrow(citations)){

      shinyalert("Unique identifier generated!",
                 "The record_id column within uploaded citations was not unique. ASySD has generated a unique record_id for each citation. You can preview this in the table.", type = "info")

      citations <-  citations %>%
        mutate(record_id =  as.character(row_number()+1000))

      rv$refdata <- citations
    } else{
      rv$refdata <- citations
    }

    citation_summary <- rv$refdata  %>%
      select(source, label, file_name) %>%
      group_by(file_name) %>%
      add_count(name = "number_of_citations") %>%
      group_by(file_name, number_of_citations) %>%
      distinct() %>%
      summarise(source = ifelse(all(is.na(source)), "", paste(ifelse(!is.na(source) | source != "", source, NA), collapse = ", ")),
                label = ifelse(all(is.na(label)), "", paste(ifelse(!is.na(label) | label != "", label, NA), collapse = ", "))) %>%
      mutate(source = ifelse(str_length(source) > 10, "multiple", source),
             label = ifelse(str_length(label) > 10, "multiple", label)) %>%
      select(file_name, number_of_citations, label, source)

    rv$citation_summary <- citation_summary

    }
})

    # when file upload table is edited, edit reactive value upload df
    observeEvent(input$summary_table_cell_edit, {
        row  <- input$summary_table_cell_edit$row
        clmn <- input$summary_table_cell_edit$col + 1

        file_to_change <- rv$citation_summary[row, 1][[1]]
        value_to_change <- rv$citation_summary[row, clmn][[1]]
        col_to_change <- names(rv$citation_summary[row, clmn])
        rv$citation_summary[row, clmn] <- input$summary_table_cell_edit$value

        df <- rv$refdata

        df[[col_to_change]][df$file_name == file_to_change] <- input$summary_table_cell_edit$value

        rv$refdata <- df
    })


  # Get ref ID choice
  output$Id_col_picker <- renderUI({

    selectInput(
      inputId = "Id_col_picker",
      label = "Select a column which contains the unique ID for each citation",
      choices = (unique(names(rv$refdata))), # col names in ref data
      selected = "record_id"
    )
  })


  # when citations uploaded, render unique citation picker
  observeEvent(input$uploadfile, {

    output$Keep_citation_picker <- renderUI({

      shinyWidgets::prettyRadioButtons(
        inputId = "Keep_citation_picker",
        label = "If you uploaded citations with a unique record_id, one of these will be chosen as the duplicate_id for each group of duplicates. If you select a certain preference below, this record_id will be chosen over others.
        These identifiers are helpful for maintaining good records of citations throughout a systematic review.
        Note that ASySD merges records to retain meta-data (e.g. DOIs present in one citation and not the other) where possible.",
        choices = c("Citation with abstract", "Citation with a specific label", "Citation from a specific source"),
        selected = c("Citation with abstract"),
        status="primary")
    })

  })

  # Get choices for keep label
  output$keepLabel <- renderUI({

    shiny::validate(
      shiny::need(input$Keep_citation_picker != "", "")
    )


    if (input$Keep_citation_picker == "Citation with a specific label") {

      selectInput(inputId = "keepLabel",
                  label = "Specify labelled references to keep in the dataset",
                  choices = c("", unique(rv$refdata$label)),
                  selected = NULL,
                  multiple = FALSE)
    }
  })


  # Get choices for keep label
  output$keepSource <- renderUI({

    shiny::validate(
      shiny::need(input$Keep_citation_picker != "", "")
    )


    if (input$Keep_citation_picker == "Citation from a specific source") {

      selectInput(inputId = "keepSource",
                  label = "Specify source to keep in the dataset",
                  choices = unique(rv$refdata$source),
                  selected = NULL,
                  multiple = FALSE)
    }
  })


  # Datatable summarising upload ----
  output$citation_table <- renderDT({

    shiny::validate(
      shiny::need(!is.null(rv$refdata), ""))

    preview <-  rv$refdata %>%
      dplyr::select(record_id, author, title, year, journal, abstract, doi, number, pages, volume, isbn, label, source)

    DT::datatable(rv$refdata,
                  options = list(scrollX = TRUE,
                                 pageLength = 10,
                                 fixedColumns = TRUE,
                                 columnDefs = list(list(
                                   targets = "_all",
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type === 'display' && data != null && data.length > 20 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                     "}")
                                 ))),

                  rownames = FALSE,
                  class = "display")

  })


  output$missing_data_table <- renderDT({


    shiny::validate(
      shiny::need(!is.null(rv$refdata), ""))

     check_cols <- c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "isbn", "label", "source")
     subset_data <- subset(rv$refdata, select = check_cols)
     missing_percentages <- colSums(is.na(subset_data)) / nrow(subset_data) * 100
     # Create a data frame with the missing percentages for each column
     missing_table <- data.frame(
       field = names(missing_percentages),
       percentage_missing = round(missing_percentages, 1))

     DT::datatable(missing_table,
                   options = list(
                     searching = FALSE,
                     lengthChange = FALSE,
                     pageLength = 20
                   ),
                   rownames = FALSE
     ) %>%
       formatStyle(
         "percentage_missing",
         target = "cell",
         backgroundColor = styleInterval(
           c(0, 25, 50, 75),
           c("yellowgreen", "yellowgreen", "orange", "orange", "red")
         ),
         color = "white",
         fontWeight = "bold",
         borderRadius = "2px",
         border = "2px solid",
         borderColor = styleInterval(
           c(0, 25, 50, 75),
           c("yellowgreen", "yellowgreen", "orange", "orange", "red")
         ),
         padding = "3px",
         display = "flex"
       )
  })


  output$summary_table <- renderDT({

    DT::datatable(rv$citation_summary,
                  options = list(
                    searching = FALSE,
                    lengthChange = FALSE
                  ),
                  rownames = FALSE,
                  class = "display",
                  editable = TRUE
    )
  })

  # ASySD r text ----
  output$Post_upload_text <- renderText({

    original_refs_n <- as.numeric(nrow(rv$refdata))

    shiny::validate(
      shiny::need(!length(original_refs_n) == 0, "")
    )

    paste("You have uploaded", original_refs_n, "citations. Preview them below.")

  })

  # output$ASySD_pre_upload <- renderUI({
  #
  #   shiny::validate(
  #     shiny::need(input$fileType != "", "")
  #   )
  #
  #   if (input$fileType == "Endnote XML") {
  #
  #     str1 <- paste("Formatting requirements:")
  #     str2 <- paste("From Endnote or Zotero, select references and export to an XML file")
  #
  #     HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
  #
  #   }
  #
  #   else if (input$fileType == "CSV") {
  #
  #     str1 <- paste("Formatting requirements:")
  #     str2 <- paste("Within excel or another program, ensure your data has the following columns:
  #     author, year, journal, doi, title, pages, volume, number,
  #     abstract, record_id, isbn, label. The column order does not matter. Some columns can be blank,
  #     but as much metadata as possible will aid deduplication")
  #
  #     HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
  #   }
  #   else if (input$fileType == "Tab delimited") {
  #
  #     str1 <- paste("Formatting requirements:")
  #     str2 <- paste("From Endnote, select references and export to an tab delimited (.txt) file")
  #
  #     HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
  #   }
  # })

  # download types info
  output$ASySD_download_type <- renderUI({

    shiny::validate(
      shiny::need(input$export_type != "", "")
    )

    if (input$export_type == "unique_only_export") {

      str1 <- paste("Export type:")
      str2 <- paste("This export will contain only unique citations after automated deduplication in ASySD. If you removed any additional duplicates using the manual
      deduplication option, this export will reflect this. If you flagged potential duplicates for later review,
                    this will also appear in the export. During the deduplication process, a unique identifier (duplicate_id) is generated for each duplicate set. This number will also be present in the export,
                    with a unique number for each citation. Exact column names vary depending on export format (see below).")
      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))

    } else {

      str1 <- paste("Export type:")
      str2 <- paste("This export will include all citations uploaded in your initial dataset. Groups of duplicates identified by ASySD are indicated by the duplicate_id i.e. multiple citations which match will have the same duplicate_id.")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }
  })

  # download formatting info
  output$ASySD_pre_download <- renderUI({

    shiny::validate(
      shiny::need(input$export_format != "", "")
    )

    if (input$export_format == "txt") {

      str1 <- paste("Export format notes:")
      str2 <- paste("Custom 1 = duplicate_id")
      str3 <- paste("Custom 2 = flags for potential duplicates (if this option was selected in manual deduplication).")
      str4 <- paste("Database name = source(s)")
      str5 <- paste("Label = label(s)")
      str6 <- paste("To import into Endnote, make sure to select the Tab Delimited input option")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>", str3, "<br>", str4,
                 "<br>", str5, "<br>", str6))

    }

    else if (input$export_format == "ris") {

      str1 <- paste("Formatting notes for RIS import to Endnote:")
      str2 <- paste("Notes = flags for potential duplicates (if this option was selected in manual deduplication).\n")
      str3 <- paste("Database Name = duplicate_id\n")

      str4 <- paste("Formatting notes for RIS import to Zotero:")
      str5 <- paste("Notes = flags for potential duplicates (if this option was selected in manual deduplication).\n")
      str6 <- paste("Archive = duplicate_id\n")

      str7 <- paste("Formatting notes for RIS import to Mendeley:")
      str8 <- paste("Unfortunately, duplicate identifiers and flags for potential duplicates are not imported into Mendeley. If there is enough demand, we will aim to work on a solution for this.\n")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>", str3, "<br>", "<br>",
                 "<b>", str4, "</b>", "<br>", str5, "<br>", str6, "<br>", "<br>",
                 "<b>", str7, "</b>", "<br>", str8, "<br>"))

    }
    else if (input$export_format == "syrf_csv") {
      str1 <- paste("Export format notes:")
      str2 <- paste("At present this export does not contain any custom fields to retain potential duplicate flags as this is intended for direct input into SyRF (you should only upload unique citations lists).")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }

    else if (input$export_format == "csv") {
      str1 <- paste("Export format notes:")
      str2 <- paste("This is a basic export with all field names. This is useful for understanding the data structure
                    and output from ASySD.")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }

    # else if (input$export_format == "bib") {
    #
    #   str1 <- paste("Export format notes:")
    #   str2 <- paste("Notes = flags for potential duplicates (if this option was selected in manual deduplication).")
    #   str3 <- paste("Database Name = duplicate_id")
    #
    #   HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>", str3))
    # }
  })

  # ASySD user text ----
  output$ASySD_pretext <- renderText({
    paste("Configure the options below, then click the button to automatically
remove duplicates.")

  })

  # Action: fomrat citations to dedup ----
  citations_to_dedup <- eventReactive(input$dedupbutton,{

    id_col <- sym(input$Id_col_picker)
    id_col <- enquo(id_col)

    citations <- rv$refdata %>%
      mutate(record_id = !!id_col)

  })

  # Action: ASySD auto dedup ----
  auto_dedup_result <- eventReactive(input$dedupbutton,{

    if(length(input$keepLabel) > 1){

     if(input$keepLabel == "") {
      keep_label <- NULL
    } else {
      keep_label <- input$keepLabel
    }

    } else {

      keep_label <- NULL
  }


    result <- ASySD::dedup_citations(citations_to_dedup(),
                              keep_source = input$keepSource,
                              keep_label = keep_label,
                              merge_citations = TRUE,
                              shiny_progress =TRUE)

    rv$pairs_to_check <- result$manual_dedup %>%
      mutate(" " = "") %>%
      select(" ", everything())

    rv$latest_unique <- result$unique

    return(result)
  })


  # Output: ASySD auto dedup results text ----
  output$ASySD_results <- renderText({

    uniquerefs_n <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    original_refs_n <- as.numeric(nrow(rv$refdata))
    removed_n <- original_refs_n - uniquerefs_n
    manual_pairs_n <- as.numeric(length(auto_dedup_result()$manual$record_id1))

    paste("<font color=\"#565656\"><b>", "From a total of", original_refs_n, "citations, ASySD has removed ", "<font color=\"#FF0000\"><b>", removed_n,  "<font color=\"#565656\">", "duplicates.",
          "There are", uniquerefs_n, "remaining.", manual_pairs_n, "possible duplicate pairs have been flagged for manual deduplication. Go to the
          manual deduplication tab to check these suggested duplicates.")

  })

  # Action: remove manually selected duplicates ----
  manual_dedup_result <- eventReactive(input$manualdedupsubmit,{

    after <- dedup_citations_add_manual(rv$latest_unique,
                                        merge_citations = TRUE,
                                        additional_pairs = rv$pairs_removed,
                                        keep_source = input$keepSource,
                                        keep_label = input$keepLabel)
    # update latest unique df
    rv$latest_unique <- after
  })

#
#   manual_dedup_result_flagged <- eventReactive(input$manualdedupflag,{
#
#     # add flag to relevant citations
#     unique_citations <- rv$latest_unique %>%
#       mutate(flag = ifelse(duplicate_id %in% c(rv$pairs_flagged$record_id1, rv$pairs_flagged$record_id2), "potential_duplicates", ""))
#
#     rv$latest_unique <- unique_citations
#     })
#
#   manual_dedup_result_flagged_all <- eventReactive(input$manualdedupflag_all,{
#
#     unique_citations <- rv$latest_unique %>%
#       mutate(flag = ifelse(duplicate_id %in% c(rv$pairs_flagged$record_id1, rv$pairs_flagged$record_id2), "potential_duplicates", ""))
#
#     rv$latest_unique <- unique_citations
#   })


  # Action: ASySD manual dedup pre text ----
  output$Manual_pretext <- renderText({

    manualrefs <- auto_dedup_result()$manual
    manualrefs <- as.numeric(length(manualrefs$record_id1))

    paste(manualrefs, "pairs of citations (see table below) require manual review. You can scroll right to see all citation metadata and hover over any cell to see truncated text. Identical and near-identical fields are highlighted in green.
        Select all rows which contain duplicate pairs and click the button below to remove extra
        duplicates.")

  })

  # Action: ASySD manual dedup results text ----
  output$Manual_results <- renderText({

    if(input$manualdedupsubmit >0){

    unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id))
    manualremove <-  as.numeric(length(auto_dedup_result()$unique$duplicate_id)) - unique_manual

    paste(manualremove, "additional citations were removed manually.", unique_manual, "unique citations now remain.")

  }

    else if(!input$manualdedupflag == 0){

      "Selected pairs flagged in output file for future review. See downloads page for further details."
    }
    else if(!input$manualdedupflag_all == 0){

      "All pairs flagged in output file for future review. See downloads page for further details."
    } else{

      ""
    }

      })

  # Output: manual pairs to be checked

  observeEvent(input$manualdedupsubmit, {

    rv$pairs_removed <- rv$pairs_to_check[input$manual_dedup_dt_rows_selected,]
    rv$pairs_to_check <- rv$pairs_to_check[-input$manual_dedup_dt_rows_selected,]

    if(nrow(rv$pairs_removed) < 1){
      shinyalert("Oops!", "You haven't selected any duplicate pairs to remove.", type = "error")
      return()
    }

  })

    observeEvent(input$manualdedupflag, {

    rv$pairs_flagged <- rv$pairs_to_check[input$manual_dedup_dt_rows_selected,]
    rv$pairs_to_check <-  rv$pairs_to_check %>%
      mutate(" " = ifelse(record_id1 %in% rv$pairs_flagged$record_id1 &
                                    record_id2 %in% rv$pairs_flagged$record_id2, as.character(icon("flag")), ""))


    })

    observeEvent(input$manualdedupflag_all, {

      rv$pairs_flagged <- rv$pairs_to_check
      rv$pairs_to_check <-  rv$pairs_to_check %>%
        mutate(" " = as.character(icon("flag")))


    })


  # Output: manual dedup warning if no pairs  -----

    observeEvent(input$dedup_pages,{
      if(input$dedup_pages == "Manual deduplication"){
        if(length(rv$pairs_to_check$record_id1>1)){
          return()

          }else if(length(rv$pairs_to_check$record_id==0)){
          shinyalert("No manual deduplication required!",
                     "There are no additional suggested pairs for manual deduplication", type = "info")

             }else{
        shinyalert("No pairs identified yet...",
                   "Run the automated deduplication first in the previous tab!", type = "warning")
          }

      }
    })

  # Output: manual dedup datatable -----
  output$manual_dedup_dt <- renderDT(
    datatable(rv$pairs_to_check,
              escape=FALSE,
              options = list(dom = 'tp',
                             pageLength = 10,
                             fixedColumns = TRUE,
                             scrollX = TRUE,
                             columnDefs =
                               list(list(visible=FALSE, targets=c(4, 7, 10, 13, 16, 19, 22, 25, 26, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40)),
                             list(targets = c(2,3,4,5,6,7,8,9,10,11),
                            render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data != null && data.length > 25 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                                                   "}")
                                               )))) %>%
      formatStyle(
        c('title1', 'author1', 'volume1',
          'pages1', 'number1', 'year1', 'abstract1', 'journal1', 'isbn1'),
        c('title', 'author', 'volume',
          'pages', 'number', 'year', 'abstract', 'journal', 'isbn'),
        backgroundColor = styleInterval(c(0.95, 1), c('white', '#82d173', '#82d173'))) %>%
      formatStyle(
        c('title2', 'author2', 'volume2',
          'pages2', 'number2', 'year2', 'abstract2', 'journal2', 'isbn2'),
        c('title', 'author', 'volume',
          'pages', 'number', 'year', 'abstract', 'journal', 'isbn'),
        backgroundColor = styleInterval(c(0.95, 1), c('white', '#82d173', '#82d173'))) %>%
      formatStyle(
        c('doi1'),
        c('doi'),
        backgroundColor = styleInterval(c(0.999, 1), c('white', '#82d173', '#82d173'))) %>%
      formatStyle(
        c('doi2'),
        c('doi'),
        backgroundColor = styleInterval(c(0.999, 1), c('white', '#82d173', '#82d173')))
  )



  # Output: sankey diagram ---
  output$sankey <- renderSankeyNetwork({

    n_search <- original_refs_n <- as.numeric(nrow(rv$refdata))
    n_unique_auto <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    try(n_unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id)), silent=TRUE)

    if(exists("n_unique_manual")){

      final_unique  <- as.numeric(length(rv$latest_unique$duplicate_id))

    links <- data.frame(source =
                          c(paste0("Original citations (", n_search, ")"),
                            paste0("Original citations (", n_search, ")"),
                            paste0("Remaining citations (", n_unique_auto, ")"),
                            paste0("Remaining citations (", n_unique_auto, ")")),
                        target =
                          c(paste0("Remaining citations (", n_unique_auto, ")"),
                            paste0("Auto-dedup removed (", n_search - n_unique_auto, ")"),
                            paste0("Unique citations (", final_unique, ")"),
                            paste0("Manually removed (", n_unique_auto - final_unique, ")")),
                        value = c(n_search, n_search - n_unique_auto, final_unique, n_unique_auto - final_unique))

    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(
      name=c(as.character(links$source),
             as.character(links$target)) %>% unique()
    )

    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    # Make the Network
    p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name",
                       sinksRight=FALSE, nodeWidth =30, height = 100, width = 800, fontSize = 14,
                       units = "Citations")
    p

    }

    else{


      links <- data.frame(source =
                            c(paste0("Original citations (", n_search, ")"),
                              paste0("Original citations (", n_search, ")")),
                            target =
                            c(paste0("Unique citations (", n_unique_auto, ")"),
                              paste0("Auto-dedup removed (", n_search - n_unique_auto, ")")),
                          value = c(n_unique_auto,  n_search - n_unique_auto))

      # From these flows we need to create a node data frame: it lists every entities involved in the flow
      nodes <- data.frame(
        name=c(as.character(links$source),
               as.character(links$target)) %>% unique()
      )

      # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
      links$IDsource <- match(links$source, nodes$name)-1
      links$IDtarget <- match(links$target, nodes$name)-1

      # Make the Network
      p <- sankeyNetwork(Links = links, Nodes = nodes,
                         Source = "IDsource", Target = "IDtarget",
                         Value = "value", NodeID = "name",
                         sinksRight=FALSE, nodeWidth =30, height = 100, width = 800, fontSize = 14,
                         units = "Citations")
      p

    }
  })

  final_results <- reactive({


    unique <- rv$latest_unique
    dup_ids_to_flag <- unique %>%
      select(record_ids, duplicate_id) %>%
      group_by(duplicate_id) %>%
      tidyr::separate_rows(record_ids, sep=", ") %>%
      mutate(flag = ifelse(record_ids %in% c(rv$pairs_flagged$record_id1, rv$pairs_flagged$record_id2), TRUE, FALSE)) %>%
      filter(flag == TRUE) %>%
      select(duplicate_id)

    unique <- unique %>%
      mutate(flag = ifelse(duplicate_id %in% dup_ids_to_flag$duplicate_id, "potential_duplicates", ""))

    if(input$export_type == "all_citations_export"){

        final <- unique %>%
          select(flag, duplicate_id, record_ids) %>%
          rename(record_id = record_ids) %>%
          tidyr::separate_rows(record_id)

        final <- left_join(final, citations_to_dedup(), by="record_id")

      }  else{

          final <- unique %>%
            select(flag, duplicate_id) %>%
            mutate(record_id = duplicate_id)


          unique <- unique %>%
          select(duplicate_id, source, label)
          final <- left_join(final, citations_to_dedup(), by="record_id")

        final <- final %>%
          select(-source, -label)

        final <- left_join(unique, final, by="duplicate_id")
      }

    if(input$filterLabelOnly == TRUE){

    final <- final %>%
      mutate(label = ifelse(is.na(label), "NA", label)) %>%
      filter(label %in% input$filterLabel)

    } else{

      if(all(input$filterLabel==" ")){

        final <- final

      }

      else if(all(input$filterLabel=="NA")){

        final <- final

      }

    else{

      final <- final %>%
        filter(grepl(paste0("\\b", paste(input$filterLabel,collapse = "\\b|\\b"), "\\b|\\bunknown\\b"), source))
      }
    }


    if(input$filterSourceOnly == TRUE){

      final <- final %>%
        mutate(source = ifelse(is.na(source), "NA", source)) %>%
        filter(source %in% input$filterSource)

    } else{

      if(all(input$filterSource==" ")){

        final <- final

      }  else if(all(input$filterSource=="NA")){

        final <- final

      } else{

      final <- final %>%
        filter(grepl(paste0("\\b", paste(input$filterSource,collapse = "\\b|\\b"), "\\b|\\bunknown\\b"), source))
      }
    }
    return(final)
  })

  # when citations uploaded, get list of labels and filter by this in output
  output$filterLabel <- renderUI({

    selectInput(inputId = "filterLabel",
                  label = "Citations with these labels will be exported",
                  choices = unique(rv$refdata$label),
                  selected = unique(rv$refdata$label),
                  multiple = TRUE)
  })

  # when citations uploaded, get list of sources and filter by this in output
  output$filterSource <- renderUI({

    selectInput(inputId = "filterSource",
                label = "Citations with these sources will be exported",
                choices = unique(rv$refdata$source),
                selected = unique(rv$refdata$source),
                multiple = TRUE)
  })




    # output: download unique citations - endnote
    output$download<- downloadHandler(

        filename = function() {
          paste0(input$export_type, ".", input$export_format)
        },

        content = function(file) {


        write_citations_app(final_results(), type = input$export_format, file)
        }
    )

}
# Run the application
shinyApp(ui = ui, server = server)



