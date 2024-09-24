# Derived from revtools code, copyright Martin J Westgate
# Adapted by Lukas Wallrich, still unter GPL-3 licence

add_green <- function(x) {
  paste('<span style="color: green">', x, '</span>')
}

clean_text <- function(str) {
  str %>%
    # Remove editions like "2nd Ed."
    gsub("(\\d+(?:st|nd|rd|th)\\s*Ed[\\. ])", "", .) %>%
    # Remove reference numbers like "[2]"
    gsub("\\[\\d+\\]", "", .) %>%
    # Remove HTML tags
    gsub("<.*?>", "", .) %>%
    # Remove HTML encoded tags
    gsub("&lt;.*?&gt;", "", .) %>%
    # Remove copyright statements
    gsub("Copyright (?:of the )?(.*?)(, all rights reserved\\.)?\\.", "", .)
}

format_overviews <- function(
    x
){
    score_row <- x %>% dplyr::filter(is.na(.data$suffix))
    x <- x %>% dplyr::filter(!is.na(suffix))

    cols <- setdiff(names(x), "suffix")
    labels <- tools::toTitleCase(setdiff(names(x), "suffix"))

    # Create the main output table with the two records and similar scores
    out <- dplyr::tibble(
      " " = paste("<b>", labels, "</b>"),
      !!(x[[1, "suffix"]]) := c(t(x[1, cols])),
      !!(x[[2, "suffix"]]) := c(t(x[2, cols])),
      "Score" = round(as.numeric(c(t(score_row[, cols]))), 2)
    )

    matches <- out %>%
      dplyr::mutate(clean = clean_text(.[[2]]) == clean_text(.[[3]])) %>%
      dplyr::pull(.data$clean)

    matches[is.na(matches)] <- FALSE

    out %>% dplyr::mutate(across(everything(), ~ifelse(matches, add_green(.x), .x)))

}

#' A Shiny interface to review potential duplicates
#'
#' `dedup_citations()` can return potential duplicates for manual review. This
#' function takes these potential duplicates and provides a Shiny interface to
#' review them and select those that should be deduplicated. The output can then
#' be passed to `dedup_citations_add_manual()` to complete the deduplication, or
#' be used to call this function again if manual review is not yet complete.
#'
#' @export
#' @importFrom rlang .data
#' @param manual_dedup The dataframe with potential duplicates returned from `dedup_citations`
#' @return The dataframe with a `result` column indicating whether the entry
#' constitutes a duplicate - to be passed to `dedup_citations_add_manual()`
#' @examplesIf interactive()
#'
#' # Perform deduplication
#' result <- dedup_citations(citations_df, keep_source="Embase")
#'
#' # Manually review potential duplicates
#' manual_review <- manual_dedup_shiny(result$manual_dedup)
#'
#' # Complete deduplication
#' final_result <- dedup_citations_add_manual(result$unique, additional_pairs = manual_review)

manual_dedup_shiny <- function(
    df, cols = names(df)
){

  id = "record_ids"

  suffixes = c("1", "2")

  input_data <- list(
    raw = NULL,
    wip = NULL
  )

  #### PROCESS DATA

  # make sure added data has a unique column called 'id'
  if (! id %in% names(df)) df[id] <- 1:nrow(df)
  if (! "result" %in% names(df))
  {df$result <- NA} else {
    if(all(!is.na(df$result))) stop("Nothing to be done here - all entries have a result")
    message("Only entries where result is NA will be displayed")
  }
  if (length(df[id]) != length(unique(df[id]))) stop("Ensure that ", id, " is a unique identifier in df.")
  # if (length(unique(suffixes)) != 2) stop("Only two sources - i.e. suffixes - can be considered.")

  input_data$raw <- df

  if (!any(stringr::str_detect(cols, paste0(suffixes, collapse = "|"))))
    cols = c(id, as.vector(outer(cols, suffixes, paste, sep="")))

  names(df) <- names(df) %>% stringr::str_replace_all(paste0("___", suffixes) %>% rlang::set_names(paste0(suffixes)))
  cols <- cols %>% stringr::str_replace_all(paste0("___", suffixes) %>% rlang::set_names(paste0(suffixes)))
  relevant_cols <- c(id, "result", cols %>% stringr::str_subset(paste0(paste0("___", suffixes), collapse = "|")))

  # Identify score columns (those without suffixes, but related to columns with suffixes)
  suffix_columns <- cols %>% stringr::str_subset(paste0(paste0("___", suffixes), collapse = "|"))
  basenames <- unique(stringr::str_remove(suffix_columns, paste0("___", suffixes, collapse = "|")))
  score_cols <- basenames[basenames %in% names(df)]

  # Add score columns to the set of relevant columns
  relevant_cols <- c(id, "result", cols %>% stringr::str_subset(paste0(paste0("___", suffixes), collapse = "|")), score_cols)

  # if(length(setdiff(cols, relevant_cols))>0)
  #   message("The following colums are ignored: ",
  #           paste(setdiff(cols, relevant_cols), sep = ", "),
  #           ". Only colums ending with the suffixes are considered.")

  df <- df %>% dplyr::select(dplyr::any_of(relevant_cols), "result") %>%
    dplyr::filter(is.na(.data$result)) %>%
    mutate(across(.fns = as.character)) %>%
    tidyr::pivot_longer(c(-id, -result)) %>%
    tidyr::separate_wider_delim(name, names = c("var", "suffix"), delim = "___", too_few = "align_start") %>%
    tidyr::pivot_wider(names_from = .data$var, values_from = .data$value)

  input_data$wip <- df

  #### CREATE APP

  # create ui
  ui <- screen_matches_ui(suffixes)

  # start server
  server <- function(input, output, session){

    # reactive values
    data <- reactiveValues(
      raw = input_data$raw,
      wip = input_data$wip,
      done = input_data$wip[0,]
    )

    progress <- reactiveValues(
      entry = input_data$wip[[id]][1],
      done = NULL
    )

    # action buttons
    output$selector_bar <- renderUI({
      text_out <- HTML(
        paste0("Dataset with ",
               nrow(data$raw),
               " entries  |  ",
               nrow(data$wip)/3,
               " remaining to be checked. "
        )
      )
      div(
        div(class = "container",
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  flex-grow: 1;",
              renderText({text_out})
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 20px",
              renderText(" ")
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_skip",
                label = HTML("Skip<br><i>[space]</i>"),
                width = "100px",
                style = "background-color: #darkgrey;height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_match",
                label = HTML("Duplicate<br><i>[1]</i>"),
                width = "100px",
                style = "background-color: #006400;height: 45px; padding:1px;"
              )
            ),

            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "selected_no_match",
                label = HTML("NO duplicate<br> <i>[3]</i>"),
                width = "100px",
                style = "background-color: #8B008B; height: 45px; padding:1px;"
              )
            ),
            div(
              style = "
                  display: inline-block;
                  vertical-align: center;
                  text-align: right;
                  width: 110px",
              actionButton(
                inputId = "undo",
                label = "Undo",
                icon = icon("backspace"),
                width = "100px",
                style = "background-color: #darkgrey; height: 45px; padding:1px;"
              )
            )
        )
      )
    }
    )

    # summaries
    output$hit <- renderTable({
      shiny::validate(
        need(nrow(data$wip) > 0, "")
      )
      format_overviews(
        x = data$wip %>%
          dplyr::filter(!!as.symbol(id) == progress$entry) %>%
          dplyr::select(-!!as.symbol(id), -"result")
      ) %>%
        filter(" " != "Record_id")},
      width = NULL,
      striped = TRUE,
      sanitize.text.function=function(x){x},
      type = "html"
    )

    # Move through results
    show_next <- function() {
      data$done <- bind_rows(data$done, data$wip[data$wip[[id]] == progress$entry,])
      progress$done <- c(progress$entry, progress$done)
      data$wip <- data$wip[data$wip[[id]] != progress$entry,]
      if(nrow(data$wip) == 0){
        progress$entry <- NULL
        showModal(modalDialog(
          title = "Screening complete - well done!",
          if (sum(is.na(isolate(data$raw$result)))>0) {paste0("Skipped ", sum(is.na(isolate(data$raw$result))), ".
                                                                    You will need to relaunch the app (passing the updated data) to deal with them.")
          } else {""},
          "You can close the app now. Data will be returned to the workspace. If you have already assigned it to an object, it will be saved there. Otherwise, you have to save it with df <- .Last.value"))

      } else {
        progress$entry <- data$wip[[id]][1]
      }
    }

    # respond when buttons are triggered
    observeEvent(input$selected_skip, {
      data$raw$result[data$raw[[id]] == progress$entry] <- NA
      show_next()
    })

    observeEvent(input$selected_match, {
      data$raw$result[data$raw[[id]] == progress$entry] <- "match"
      show_next()
    })

    observeEvent(input$selected_no_match, {
      data$raw$result[data$raw[[id]] == progress$entry] <- "no_match"
      show_next()
    })

    observeEvent(input$undo, {
      if(nrow(data$done)==0) {
        warning("Nothing to undo")
        return(NULL)}
      progress$entry <- progress$done[1]
      progress$done <- progress$done[-1]
      data$wip <- bind_rows(data$done[data$done[[id]] == progress$entry,], data$wip)
      data$done <- data$done[data$done[[id]] != progress$entry,]
    })

    observeEvent(input$keys, {
      switch (input$keys,
              "1" = shinyjs::click("selected_match"),
              "3" = shinyjs::click("selected_no_match"),
              "space" = shinyjs::click("selected_skip"),
              "backspace" = shinyjs::click("undo")
      )
    })



    onStop(function() {
      message("Returning results")
      stopApp(returnValue = isolate(data$raw))
    })

  } # end server

  app <- shinyApp(ui, server)
  runApp(app)

}

screen_matches_ui <- function(suffixes){

  # build user interface
  fluidPage(
    keys::useKeys(),
    shinyjs::useShinyjs(),
    keys::keysInput("keys", c("1", "2", "3", "space", "backspace")),
    tags$head(tags$style(HTML('
    .container {display: flex; width: 100%}
    #hit table {
                               width: 100%;
                             }
                             #hit td:nth-child(2) {
                               width: 40%;
                             }
                             #hit td:nth-child(3) {
                               width: 40%;
                             }'))),
    theme = shinythemes::shinytheme("cosmo"),
    titlePanel("ASySD | Review potential duplicates"),

    fluidRow(
      uiOutput("selector_bar")
    ),
    fluidRow(
      column(
        width = 12,
        tableOutput("hit")
      )
    )
  )
}



