# R/srv_data_loader.R

library(shiny)

srv_data_loader <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Reactives (will hold data later) ---
    # raw_data <- reactiveVal(NULL)
    # processed_data <- reactiveVal(NULL)
    # filtered_data <- reactiveVal(NULL)

    # --- Outputs (Placeholders for now) ---
    output$file_name_display <- renderText({
      # Later: update based on input$file_in
      "[ loaded filename here, ellipsis-truncated if needed ]"
    })

    output$feature_count_display <- renderUI({
      # Later: update based on raw_data()
      HTML(paste0("<b>", format(50000 + sample(1:1000,1), big.mark=","), "</b> features"))
    })

    output$fdr_count_display <- renderUI({
       # Later: update based on raw_data()
       HTML(paste0("FDR < .05: <b>", format(300 + sample(1:100,1), big.mark=","), "</b>"))
    })

    output$selected_count_display <- renderUI({
       # Later: update based on filtered_data() and FDR count within selection
       sel_count <- 30000 + sample(1:1000,1)
       fdr_count <- 300 + sample(1:100,1)
       HTML(paste0("Selected: <b>", format(sel_count, big.mark=","), "</b> features | FDR < .05: <b>", fdr_count, "</b>"))
    })

    # --- Observers (for linking slider/numeric inputs) ---
    observeEvent(input$max_p_val_num, {
      updateSliderInput(session, "max_p_val_sli", value = input$max_p_val_num)
    })

    observeEvent(input$max_p_val_sli, {
      updateNumericInput(session, "max_p_val_num", value = input$max_p_val_sli)
    })

    # --- Return list (will contain reactives later) ---
    # return(
    #   list(
    #     raw = raw_data,
    #     processed = processed_data,
    #     filtered = filtered_data,
    #     label = reactive(input$label_in)
    #   )
    # )
    return(list(label = reactive(input$label_in))) # Minimal return for now
  })
}
