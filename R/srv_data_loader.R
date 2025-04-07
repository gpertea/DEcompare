# R/srv_data_loader.R

library(shiny)
library(data.table)

srv_data_loader <- function(id, default_label = "DE-results-X") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactives ---
    loaded_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)
    filtered_data <- reactiveVal(NULL)
    initial_counts <- reactiveVal(list(total = 0, fdr05 = 0))
    # New reactive for feature type
    feature_type <- reactiveVal("features")

    # --- Render the File Input UI ---
    # ... (remains the same) ...
    output$file_input_ui <- renderUI({
      customFileInput(ns("file_in"), label = NULL)
    })

    # --- Initialize Other Inputs on Session Start ---
    # ... (remains the same) ...
    updateTextInput(session, "label_in", value = default_label)
    updateNumericInput(session, "max_p_val_num", value = 1.00)
    updateSliderInput(session, "max_p_val_sli", value = 1.00)
    updateNumericInput(session, "top_n_in", value = 0)

    # --- File Loading Observer ---
    observeEvent(input$file_in, {
      req(input$file_in)
      file_info <- input$file_in

      # Reset state including feature type
      processed_data(NULL)
      filtered_data(NULL)
      initial_counts(list(total = 0, fdr05 = 0))
      feature_type("features") # Reset feature type
      # ... (reset inputs) ...
      updateTextInput(session, "label_in", value = default_label)
      updateNumericInput(session, "max_p_val_num", value = 1.00)
      updateNumericInput(session, "top_n_in", value = 0)

      prog <- Progress$new(session, min=0, max=1)
      on.exit(prog$close())
      prog$set(message = "Reading file...", value = 0.2)

      dt <- tryCatch({
        readFile(filepath = file_info$datapath, original_name = file_info$name)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 10, id=ns("load_err"))
        NULL
      })

      if (!is.null(dt)) {
          prog$set(message = "Processing data...", value = 0.6)
          dt_processed <- tryCatch({
              initialProcessDE(dt) # This ensures 'fid' exists and is valid
          }, error = function(e) {
              showNotification(paste("Data processing error:", e$message), type = "error", duration = 10, id=ns("proc_err"))
              NULL
          })

          if (!is.null(dt_processed) && nrow(dt_processed) > 0) { # Check if data is not empty
              prog$set(message = "Detecting feature type...", value = 0.8)
              # --- Feature Type Detection ---
              first_fid <- dt_processed$fid[1]
              if (grepl("^ENST\\d+", first_fid)) {
                  feature_type("transcripts")
              } else {
                  # Assuming genes otherwise, could add more checks (e.g., ENSG)
                  feature_type("genes")
              }
              # -----------------------------

              prog$set(message = "Calculating counts...", value = 0.9)
              processed_data(dt_processed)
              counts <- calculateInitialCounts(dt_processed)
              initial_counts(counts)
              showNotification(paste("Loaded:", file_info$name, "-", formatCount(counts$total), feature_type()), type = "message", duration = 5, id=ns("load_ok"))
          } else if (!is.null(dt_processed) && nrow(dt_processed) == 0) {
              # Handle case of valid file but no data rows
              processed_data(dt_processed) # Store empty data table
              initial_counts(list(total = 0, fdr05 = 0))
              feature_type("features") # Default if no rows
              showNotification(paste("Loaded:", file_info$name, "- 0 features"), type = "warning", duration = 5, id=ns("load_empty"))
          }
      }
    })

    # --- Filtering Reactive ---
    observe({
        req(processed_data())
        max_p <- input$max_p_val_num
        top_n <- input$top_n_in
        dt_filt <- applyFilters(processed_data(), max_p, top_n)
        filtered_data(dt_filt)
    })

    # --- UI Outputs ---
    # Update UI outputs to use feature_type()
    output$feature_count_display <- renderUI({
      counts <- initial_counts()
      # Use the reactive feature type here
      HTML(paste0("<b>", formatCount(counts$total), "</b> ", feature_type()))
    })

    output$fdr_count_display <- renderUI({
      counts <- initial_counts()
      HTML(paste0("FDR < .05: <b>", formatCount(counts$fdr05), "</b>"))
    })

    output$selected_count_display <- renderUI({
      # Require filtered data AND processed data (for feature type)
      req(filtered_data(), processed_data())

      dt_filt <- filtered_data()
      sel_count <- nrow(dt_filt)
      fdr_count <- if (sel_count > 0) sum(dt_filt$adj.P.Val < 0.05, na.rm = TRUE) else 0

      # Use the reactive feature type here
      HTML(paste0("selected: <b>", formatCount(sel_count), "</b> ", feature_type(),
                  " | FDR < .05: <b>", formatCount(fdr_count), "</b>"))
    })

    # --- Observers for linking slider/numeric inputs ---
    # ... (remain the same) ...
     observeEvent(input$max_p_val_num, {
      val_num <- input$max_p_val_num
      if (!is.null(input$max_p_val_sli) && !is.na(input$max_p_val_sli) && val_num != input$max_p_val_sli) {
          updateSliderInput(session, "max_p_val_sli", value = val_num)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$max_p_val_sli, {
       val_sli <- input$max_p_val_sli
       if (!is.null(input$max_p_val_num) && !is.na(input$max_p_val_num) && val_sli != input$max_p_val_num) {
           updateNumericInput(session, "max_p_val_num", value = val_sli)
       }
    }, ignoreInit = TRUE)

    # --- Return list of reactives ---
    # Add feature_type to the returned list
    return(
      list(
        processed = processed_data,
        filtered = filtered_data,
        label = reactive(input$label_in),
        feature_type = feature_type # Return the reactive type
      )
    )
  })
}
