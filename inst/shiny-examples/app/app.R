# Load packages ----
require(shiny)
require(readr)
require(DT)
require(stringr)
require(dplyr)
library(networkD3)
library(rsconnect)
library(RCurl)
library(shiny)
#library(ASySD)
library(shinythemes)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(bslib)

options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)
# UI ----
ui <- navbarPage(

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
  tabPanel("Upload",

           sidebarLayout(

             sidebarPanel(


               # Input: select an input type  ----
               h4("Upload citations file ", icon("upload")),

               shinyWidgets::prettyRadioButtons(
                 inputId = "fileType",
                 label = "Choose a file type",
                 inline = TRUE,
                 choices = c("Endnote XML",
                             "CSV", "Tab delimited"),
                 status="primary"),

               uiOutput("ASySD_pre_upload"),
               br(),

               # Input: select a file to upload----
               fileInput("uploadfile", "Choose a file to upload",
                         multiple = FALSE,
                         placeholder = "No file selected")
             ),

             # UI main panel -----
             mainPanel(

               h4("Preview citations"),

               textOutput("Post_upload_text"),

               # Output: datatable of citations uploaded ----
               DT::dataTableOutput("citation_table") %>% withSpinner(color="#754E9B", type=7))
           )),


  # UI auto deduplication page  ----
  tabPanel("Deduplicate",

           tabsetPanel(
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


                      h4("Duplicate pair selection"),

                      textOutput("Manual_pretext"),

                      br(),

                      # Button

                      tags$style(HTML('.bttn-unite.bttn-primary {
                      border-color: white;
                      color: white;
                      .bttn-unite.bttn-primary:after {
                      background: #5d3d61 !important;
                      }
                      .bttn-unite.bttn-primary:before {
                      background: #6c4771 !important;
                      }')),

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

                       DTOutput("manual_dedup_dt")


                        ))),

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
             choiceNames = c("Endnote tab delimited", "RIS", "SyRF CSV"),
             choiceValues = c("txt", "ris", "csv"),
             status="success"),


           h5("Customise output - labels"),

           uiOutput("filterLabel"),
           materialSwitch(
             inputId = "filterLabelOnly",
             label = "Only retain citations which are unique to this label",
             value = FALSE,
             status = "success"
           ),

           h5("Customise output - sources"),
           uiOutput("filterSource"),
           materialSwitch(
             inputId = "filterSourceOnly",
             label = "Only retain citations which are unique to this source",
             value = FALSE,
             status = "success"
           ),

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
           HTML(paste("<b>", "Customising output - labels and sources", "</b>",
                      "<br>", "You may choose to only download citations with a specific label - for example
           to indicate citations obtained from a more recent systematic search. You can use the filters on the
           right to do this. If a citation is present across several labels e.g. in the old search and the new search,
           you may want to only keep the citations which are UNIQUE to the new search i.e. the NEW citations to add to your review.
           To do this, switch on the option for 'Only retain citations which are unique to this label'. This works for sources too.", "<br>"))
  ))),

  tabPanel("About",

           fluidPage(

             p("The", strong("Automated Systematic Search Deduplication tool (AsySD)"), "allows users to input a dataset and remove duplicate publications."),

             tags$img(class = "img-responsive img-rounded center-block",
                      src="updated_logo.png",height=150,width=150, align="center"),

             p("This tool was developed in the CAMARADES group by", tags$a(href="https://www.researchgate.net/profile/Kaitlyn_Hair", "Kaitlyn Hair."),
             "If you have any questions about the tool, please raise an issue on the GitHub (see below) or email her at kaitlyn.hair@ed.ac.uk"),
             p("The record matching function underlying this tool uses the", tags$a(href="https://rdrr.io/cran/RecordLinkage/", "RecordLinkage"), "package, created by Murat Sariyar and Andreas Borg"),
             p("The code underlying this application is available on", tags$a(href="https://github.com/camaradesuk/ASySD", "GitHub")),
             p(strong("If you want to use this application for your systematic review, please cite:                          "),
               em("Hair K, Bahor Z, Macleod M, Liao J, Sena ES. The Automated Systematic Search Deduplicator (ASySD):
             a rapid, open-source, interoperable tool to remove duplicate citations in biomedical systematic reviews. bioRxiv; 2021. DOI: 10.1101/2021.05.04.442412."))

           )))

server <- function(input, output, session){


  # Citations uploaded - reactive
  RefData <- reactive({

    shiny::validate(
      shiny::need(input$uploadfile != "",
                  "No citations uploaded yet")
    )

    input$uploadfile

    isolate(

      if(input$fileType=="Endnote XML"){

        citations <- ASySD::load_search(input$uploadfile$datapath, method="endnote")

      }  else if(input$fileType == "CSV"){

        citations <- ASySD::load_search(input$uploadfile$datapath, method="csv")

      }   else{

        citations <- ASySD::load_search(input$uploadfile$datapath, method="txt")
      }
    )
  })

  # Get ref ID choice
  output$Id_col_picker <- renderUI({

    selectInput(
      inputId = "Id_col_picker",
      label = "Select a column which contains the unique ID for each citation",
      choices = (unique(names(RefData()))), # col names in ref data
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
                  choices = unique(RefData()$label),
                  selected = RefData()$label[1],
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
                  choices = unique(RefData()$source),
                  selected = RefData()$source[1],
                  multiple = FALSE)
    }
  })


  # Datatable of input data ---
  output$citation_table <- renderDT({

    preview_10 <- RefData()[c(1:10),]

    preview_10 <- preview_10 %>%
      dplyr::select(record_id, author, title, year, journal, abstract, doi, number, pages, volume, isbn, label, source)

    DT::datatable(preview_10,
                  options = list(dom = 't',
                                 scrollX = TRUE,
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

  # ASySD user text ----
  output$Post_upload_text <- renderText({

    original_refs_n <- as.numeric(nrow(RefData()))

    paste("You have uploaded", original_refs_n, "citations. Preview the first 10 below.")

  })

  output$ASySD_pre_upload <- renderUI({

    shiny::validate(
      shiny::need(input$fileType != "", "")
    )

    if (input$fileType == "Endnote XML") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("From Endnote, select references and export to an XML file")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))

    }

    else if (input$fileType == "CSV") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("Within excel or another program, ensure your data has the following columns:
      author, year, journal, doi, title, pages, volume, number,
      abstract, record_id, isbn, label. The column order does not matter. Some columns can be blank,
      but as much metadata as possible will aid deduplication")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }
    else if (input$fileType == "Tab delimited") {

      str1 <- paste("Formatting requirements:")
      str2 <- paste("From Endnote, select references and export to an tab delimited (.txt) file")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>"))
    }
  })

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

      str1 <- paste("Export format notes:")
      str2 <- paste("Notes = flags for potential duplicates (if this option was selected in manual deduplication).")
      str3 <- paste("Database Name = duplicate_id")

      HTML(paste("<b>", str1, "</b>", "<br>", str2, "<br>", str3))
    }
    else if (input$export_format == "csv") {

      str1 <- paste("Export format notes:")
      str2 <- paste("At present this export does not contain any custom fields to retain potential duplicate flags as this is intended for direct input into SyRF (you should only upload unique citations lists).")

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

    citations <- RefData() %>%
      mutate(record_id = !!id_col)

  })

  # Action: ASySD auto dedup ----
  auto_dedup_result <- eventReactive(input$dedupbutton,{

    result <- dedup_citations(citations_to_dedup(),
                              keep_source = input$keepSource,
                              keep_label = input$keepLabel,
                              merge_citations = TRUE)
    return(result)
  })


  # Output: ASySD auto dedup results text ----
  output$ASySD_results <- renderText({

    uniquerefs_n <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    original_refs_n <- as.numeric(nrow(RefData()))
    removed_n <- original_refs_n - uniquerefs_n
    manual_pairs_n <- as.numeric(length(auto_dedup_result()$manual$record_id1))

    paste("<font color=\"#565656\"><b>", "From a total of", original_refs_n, "citations, ASySD has removed ", "<font color=\"#FF0000\"><b>", removed_n,  "<font color=\"#565656\">", "duplicates.",
          "There are", uniquerefs_n, "remaining.", manual_pairs_n, "possible duplicate pairs have been flagged for manual deduplication. Go to the
          manual deduplication tab to check these suggested duplicates.")

  })

  # Action: remove manually selected duplicates ----
  manual_dedup_result <- eventReactive(input$manualdedupsubmit,{

    removeManual <- auto_dedup_result()$manual %>%
      select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2)

    duplicates <- removeManual[input$manual_dedup_dt_rows_selected,]

    unique_citations <- auto_dedup_result()$unique
    after <- dedup_citations_add_manual(citations_to_dedup(),
                                        merge_citations = TRUE,
                                        additional_pairs = duplicates)
  })


  manual_dedup_result_flagged <- eventReactive(input$manualdedupflag,{

    # get pairs to manually dedup
    removeManual <- auto_dedup_result()$manual %>%
      select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2)

    # keep selected rows
    duplicates <- removeManual[input$manual_dedup_dt_rows_selected,]

    # dedup with selected pairs as TRUE duplicates
    unique_citations <- auto_dedup_result()$unique

    # add flag to relevant citations
    unique_citations <- auto_dedup_result()$unique
    unique_citations <- unique_citations %>%
      mutate(flag = ifelse(duplicate_id %in% c(duplicates$record_id1, duplicates$record_id2), "potential_duplicates", "")) %>%
      mutate(flag = ifelse(duplicate_id %in% c(duplicates$record_id1, duplicates$record_id2), "potential_duplicates", ""))
    })

  manual_dedup_result_flagged_all <- eventReactive(input$manualdedupflag_all,{

    removeManual <- auto_dedup_result()$manual %>%
      select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2, record_id1, record_id2)

    duplicates <- removeManual

    unique_citations <- auto_dedup_result()$unique

    unique_citations <- auto_dedup_result()$unique
    unique_citations <- unique_citations %>%
      mutate(flag = ifelse(duplicate_id %in% c(duplicates$record_id1, duplicates$record_id2), "potential_duplicates", "")) %>%
      mutate(flag = ifelse(duplicate_id %in% c(duplicates$record_id1, duplicates$record_id2), "potential_duplicates", ""))

  })


  # Action: ASySD manual dedup pre text ----
  output$Manual_pretext <- renderText({

    manualrefs <- auto_dedup_result()$manual
    manualrefs <- as.numeric(length(manualrefs$record_id1))

    paste(manualrefs, "pairs of citations require manual deduplication. Review the pairs in the table
        below. You can scroll right to see all citation metadata and hover over any cell to see truncaed text. Identical and near-identical fields are highlighted in green.
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

  # Output: manual dedup datatable -----
  output$manual_dedup_dt <- renderDT(
    datatable(auto_dedup_result()$manual,
              options = list(dom = 'tp',
                             pageLength = 10,
                             fixedColumns = TRUE,
                             scrollX = TRUE,
                             columnDefs =
                               list(list(visible=FALSE, targets=c(3,6,9, 12, 15, 18, 21, 24, 25, 30, 31, 32, 33, 34,35,36)),
                             list(targets = c(1,2,3,4,5,6,7,8,9,10),
                            render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data != null && data.length > 25 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                                                   "}")
                                               )))) %>%
      formatStyle(
        c('title1', 'author1', 'doi1', 'volume1',
          'pages1', 'number1', 'year1', 'abstract1', 'journal1', 'isbn1'),
        c('title', 'author', 'doi', 'volume',
          'pages', 'number', 'year', 'abstract', 'journal', 'isbn'),
        backgroundColor = styleInterval(c(0.95, 1), c('white', '#82d173', '#82d173'))) %>%
      formatStyle(
        c('title2', 'author2', 'doi2', 'volume2',
          'pages2', 'number2', 'year2', 'abstract2', 'journal2', 'isbn2'),
        c('title', 'author', 'doi', 'volume',
          'pages', 'number', 'year', 'abstract', 'journal', 'isbn'),
        backgroundColor = styleInterval(c(0.95, 1), c('white', '#82d173', '#82d173')))
  )



  # Output: sankey diagram ---
  output$sankey <- renderSankeyNetwork({

    n_search <- original_refs_n <- as.numeric(nrow(RefData()))
    n_unique_auto <- as.numeric(length(auto_dedup_result()$unique$duplicate_id))
    try(n_unique_manual <- as.numeric(length(manual_dedup_result()$duplicate_id)), silent=TRUE)

    if(exists("n_unique_manual")){

    links <- data.frame(source =
                          c(paste0("Original citations (", n_search, ")"),
                            paste0("Original citations (", n_search, ")"),
                            paste0("Remaining citations (", n_unique_auto, ")"),
                            paste0("Remaining citations (", n_unique_auto, ")")),
                        target =
                          c(paste0("Remaining citations (", n_unique_auto, ")"),
                            paste0("Auto-dedup removed (", n_search - n_unique_auto, ")"),
                            paste0("Unique citations (", n_unique_manual, ")"),
                            paste0("Manually removed (", n_unique_auto - n_unique_manual, ")")),
                        value = c(n_search, n_search - n_unique_auto, n_unique_manual, n_unique_auto - n_unique_manual))

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


    unique <- auto_dedup_result()$unique
    try(unique <- manual_dedup_result_flagged(), silent = TRUE)
    try(unique <- manual_dedup_result_flagged_all(), silent = TRUE)
    try(unique <- manual_dedup_result(), silent = TRUE)

    if(input$export_type == "all_citations_export"){

      if("flag" %in% colnames(unique)){
        final <- unique %>%
          select(flag, duplicate_id, record_ids) %>%
          rename(record_id = record_ids) %>%
          tidyr::separate_rows(record_id)

        final <- left_join(final, citations_to_dedup(), by="record_id")
      } else {

        final <- unique %>%
          ungroup() %>%
          select(duplicate_id, record_ids) %>%
          rename(record_id = record_ids) %>%
          tidyr::separate_rows(record_id, sep=", ")

        final <- left_join(final, citations_to_dedup(), by="record_id")

      }}

      else{

        if("flag" %in% colnames(unique)){
          final <- unique %>%
            select(flag, duplicate_id) %>%
            mutate(record_id = duplicate_id)

        } else {

          final <- unique %>%
            select(duplicate_id) %>%
            mutate(record_id = duplicate_id)
        }

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

      final <- final %>%
        filter(grepl(paste0("\\b", paste(input$filterLabel,collapse = "\\b|\\b"), "\\b"), label))
    }


    if(input$filterSourceOnly == TRUE){

      final <- final %>%
        mutate(source = ifelse(is.na(source), "NA", source)) %>%
        filter(source %in% input$filterSource)

    } else{

      final <- final %>%
        filter(grepl(paste0("\\b", paste(input$filterSource,collapse = "\\b|\\b"), "\\b"), source))
    }
    return(final)
  })

  # when citations uploaded, get list of labels and filter by this in output
  output$filterLabel <- renderUI({

    selectInput(inputId = "filterLabel",
                  label = "Citations with these labels will be exported",
                  choices = unique(RefData()$label),
                  selected = unique(RefData()$label),
                  multiple = TRUE)
  })

  # when citations uploaded, get list of sources and filter by this in output
  output$filterSource <- renderUI({

    selectInput(inputId = "filterSource",
                label = "Citations with these sources will be exported",
                choices = unique(RefData()$source),
                selected = unique(RefData()$source),
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



