# R/srv_data_loader.R

library(shiny)

# We need the default label value here. We can pass it as an argument.
srv_data_loader <- function(id, default_label = "DE-results-X") { # Added default_label argument
  moduleServer(id, function(input, output, session) {


    # --- Render the File Input UI ---
    # This ensures a fresh input element is created for each session
    output$file_input_ui <- renderUI({
      # Use the namespace-aware ID ns("file_in") for the input itself
      customFileInput(session$ns("file_in"), label = NULL)
    })

    # --- Initialize other inputs on Session Start ---
    # Reset text input to its default label
    updateTextInput(session, "label_in", value = default_label)
    # Reset numeric and slider inputs for Max P value
    updateNumericInput(session, "max_p_val_num", value = 1.00)
    updateSliderInput(session, "max_p_val_sli", value = 1.00)
    # Reset numeric input for Top N
    updateNumericInput(session, "top_n_in", value = 0)

    # Note: We don't explicitly reset fileInput here, rely on browser/Shiny default
    # and ensure our internal data reactives start NULL.

    # --- Reactives (Initialize to NULL) ---
    raw_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)
    filtered_data <- reactiveVal(NULL)

    # --- Outputs (Placeholders for now) ---
    # Placeholder for filename - will be updated by fileInput event later
    # The initial display relies on the placeholder in customFileInput
    # output$file_name_display <- renderText({ ... }) # Keep previous logic if needed

    output$feature_count_display <- renderUI({
      # Later: update based on raw_data()
      # For now, static placeholder
      HTML(paste0("<b>", format(50000 + sample(1:1000,1), big.mark=","), "</b> features"))
    })

    output$fdr_count_display <- renderUI({
       # Later: update based on raw_data()
       # For now, static placeholder
       HTML(paste0("FDR < .05: <b>", format(300 + sample(1:100,1), big.mark=","), "</b>"))
    })

    output$selected_count_display <- renderUI({
       # Later: update based on filtered_data() and FDR count within selection
       # For now, static placeholder
       sel_count <- 30000 + sample(1:1000,1)
       fdr_count <- 300 + sample(1:100,1)
       # Using the new styling div from ui_data_loader.R
       HTML(paste0("Selected: <b>", format(sel_count, big.mark=","), "</b> features | FDR < .05: <b>", fdr_count, "</b>"))
    })

 # --- Observers ---
    # Observer for the dynamically generated file input
    observeEvent(input$file_in, {
      # File processing logic will go here later
      # For now, maybe just print a message
      message("File input event detected for: ", session$ns("file_in"))
      # Example: Update internal state (will be replaced by actual loading)
      # raw_data(input$file_in$name) # Store filename temporarily for testing
    })

    # Observers for linking slider/numeric inputs (keep as before)
    observeEvent(input$max_p_val_num, {
      req(input$max_p_val_num != isolate(input$max_p_val_sli))
      updateSliderInput(session, "max_p_val_sli", value = input$max_p_val_num)
    }, ignoreInit = TRUE)

    observeEvent(input$max_p_val_sli, {
      req(input$max_p_val_sli != isolate(input$max_p_val_num))
      updateNumericInput(session, "max_p_val_num", value = input$max_p_val_sli)
    }, ignoreInit = TRUE)

    # --- Return list ---
    return(
      list(
        raw = raw_data,
        processed = processed_data,
        filtered = filtered_data,
        label = reactive(input$label_in)
      )
    )
  })
}
