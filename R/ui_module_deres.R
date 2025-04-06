# R/ui_module_deres.R

# --- Libraries ---
library(shiny)
library(bslib)
library(htmltools) # For tagList, tags

# --- Module UI ---

#' UI for DE Results Input and Filtering
#'
#' Creates a card with controls for loading and filtering one DE results set.
#' Uses inputPanel for specific rows to prevent control wrapping.
#'
#' @param id Module ID.
#' @param label_default Default label for the dataset.
#' @return A UI definition (HTML tag object).
#' @export
deres_input_ui <- function(id, label_default = "DE-results") {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(paste("DE results set", sub("deres-", "", id))),
    fileInput(ns("file_input"), "Browse..",
       accept = c(".tsv", ".tab", ".txt", ".csv", ".gz", ".qs", ".rds", ".RData", ".rda"), placeholder = "Load DE results file"), # Added .csv
    textOutput(ns("fn_display")),
    hr(),
    fluidRow(
        column(6, textInput(ns("label"), "Label:", value = label_default)),
        column(6, uiOutput(ns("feat_counts_init")))
    ),
    # Revert P-value controls to fluidRow with columns
    fluidRow(
        column(6, style = "padding-right: 5px;", # Add slight padding adjustment
               numericInput(ns("max_pval"), "Max P value", value = 1.0, min = 0, max = 1, step = 0.01, width = "100%")
        ),
        column(6, style = "padding-left: 5px;", # Add slight padding adjustment
               # Vertically align slider if needed via CSS or wrapper div
               sliderInput(ns("max_pval_slider"), NULL, value = 1.0, min = 0, max = 1, step = 0.01, width = "100%")
        )
    ),
    numericInput(ns("top_n"), "Top N selection:", value = 0, min = 0, step = 1, width = "120px"),
    uiOutput(ns("feat_counts_select"))
  )
}
# --- Module Server ---

#' Server Logic for DE Results Input and Filtering
#'
#' Handles file loading, filtering for UI display, and provides reactive outputs
#' for raw data, labels, filename, error status, and filter settings.
#' Works with data.frame inputs/outputs.
#'
#' @param id Module ID.
#' @param load_deres_file_func Function to load the data (e.g., `utils::load_deres_file`).
#' @param filter_deres_func Function to filter the data (e.g., `utils::filter_deres`).
#' @return A list of reactive expressions: `raw_data`, `label`, `filename`,
#'         `error_status`, `filter_settings`.
#' @export
deres_input_server <- function(id, load_deres_file_func, filter_deres_func) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
        raw_df = NULL,      # Stores the initially loaded data.frame (sorted by t)
        filename = NULL,    # Stores the original filename
        error_msg = NULL    # Stores loading error message
        )

    # --- File Loading ---
    observeEvent(input$file_input, {
      req(input$file_input, input$file_input$datapath, input$file_input$name) # Require datapath and name
      datapath <- input$file_input$datapath
      original_name <- input$file_input$name

      shiny::showNotification(paste("Loading:", original_name), type = "message", duration = 2)
      tryCatch({
        # Pass BOTH datapath and original name to the loading function
        loaded_df <- load_deres_file_func(filepath = datapath, original_filename = original_name)

        # Reset reactive values and filters on successful load
        rv$raw_df <- loaded_df
        rv$filename <- original_name # Store original name
        rv$error_msg <- NULL

        # Reset filters
        updateNumericInput(session, "max_pval", value = 1.0)
        updateSliderInput(session, "max_pval_slider", value = 1.0)
        updateNumericInput(session, "top_n", value = 0)

        shiny::showNotification(paste("Loaded", nrow(rv$raw_df), "features from", rv$filename),
                                  type = "message", duration = 3)

      }, error = function(e) {
        rv$raw_df <- NULL
        rv$filename <- original_name # Keep original name for error context
        rv$error_msg <- paste("Failed to load:", original_name, "-", e$message)
        # Show full error from utils function
        shiny::showNotification(rv$error_msg, type = "error", duration = 10)
        # Also print error to console for more detail
        message("ERROR during file loading: ", rv$error_msg)
        print(e) # Print the full error object
      })
    }) # End observeEvent
    # --- Sync P-value Inputs ---
    observeEvent(input$max_pval, {
        req(!is.na(input$max_pval))
        current_slider_val <- isolate(input$max_pval_slider)
        if (!is.null(current_slider_val) && current_slider_val != input$max_pval) {
             updateSliderInput(session, "max_pval_slider", value = input$max_pval)
        }
    }, ignoreInit = TRUE)
    observeEvent(input$max_pval_slider, {
        req(!is.na(input$max_pval_slider))
        current_num_val <- isolate(input$max_pval)
         if (!is.null(current_num_val) && current_num_val != input$max_pval_slider) {
            updateNumericInput(session, "max_pval", value = input$max_pval_slider)
         }
    }, ignoreInit = TRUE)

    # --- UI Outputs ---
    output$fn_display <- renderText({
      fname <- rv$filename
      if (is.null(fname)) return("[ No file loaded ]")
      max_len <- 30 # Truncate filename display length
      if (nchar(fname) > max_len) fname <- paste0(substr(fname, 1, max_len - 3), "...")
      if (!is.null(rv$error_msg)) paste("[ Error loading:", fname, "]") else paste("[ Loaded:", fname, "]")
    })

    output$feat_counts_init <- renderUI({
      # Use rv$raw_df (data.frame)
      req(rv$raw_df)
      n_total <- nrow(rv$raw_df)
      # Access columns using $ or [[ ]] for data.frame
      n_fdr <- sum(rv$raw_df$adj.P.Val < 0.05, na.rm = TRUE)
      tagList(
        tags$strong(format(n_total, big.mark=",", scientific=FALSE), "features"),
        tags$span(style = "margin-left: 10px; color: green;", paste("FDR <.05:", format(n_fdr, big.mark=",", scientific=FALSE)))
      )
    })

    # --- Reactive Data Filtering (ONLY FOR UI DISPLAY WITHIN MODULE) ---
    # This reactive is now only used for the output$feat_counts_select below
    filtered_data_for_ui_re <- reactive({
      req(rv$raw_df) # Use raw_df
      max_p <- input$max_pval
      top_n <- input$top_n
      if(is.null(max_p) || is.na(max_p)) max_p <- 1.0
      if(is.null(top_n) || is.na(top_n)) top_n <- 0
      # Use the filter function which now expects/returns data.frame
      filter_deres_func(rv$raw_df, max_p, top_n)
    })

    # Display selected feature counts (uses the internal reactive)
    output$feat_counts_select <- renderUI({
      req(rv$raw_df)
      fdata <- filtered_data_for_ui_re() # Use the internal reactive
      req(fdata) # fdata is now a data.frame
      n_selected <- nrow(fdata)
      n_fdr_selected <- sum(fdata$adj.P.Val < 0.05, na.rm = TRUE)
      tagList(
        tags$strong(style="color: #007bff;", format(n_selected, big.mark=",", scientific=FALSE), "selected features"),
        tags$span(style = "margin-left: 10px; color: green;", paste("FDR <.05:", format(n_fdr_selected, big.mark=",", scientific=FALSE)))
      )
    })

    # --- Return Values ---
    # Return reactive expressions for the main app to use
    return(list(
      # Return the raw data.frame
      raw_data = reactive({ rv$raw_df }),
      label = reactive({ input$label }),
      filename = reactive({ rv$filename }),
      error_status = reactive({ rv$error_msg }),
      # Return filter settings as a reactive list
      filter_settings = reactive({
          list(
              max_pval = input$max_pval,
              top_n = input$top_n
          )
      })
    ))
  })
}
