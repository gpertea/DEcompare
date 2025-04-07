# R/srv_cavs.R

library(shiny)
library(data.table)

MIN_OVERLAP_PCT <- 0.5

srv_cavs <- function(id, data1, data2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to track if the low overlap modal is currently shown
    low_overlap_modal_active <- reactiveVal(FALSE)

    # --- Reactive: Harmonize FIDs ---
    harmonized_pair <- reactive({
      # Require processed data from both loaders
      req(data1$processed(), data2$processed())

      dt1_proc <- data1$processed()
      dt2_proc <- data2$processed()

      # Add check to ensure data is not empty before harmonization attempt
      if(nrow(dt1_proc) == 0 || nrow(dt2_proc) == 0) {
          # Return NULL or empty list if one dataset is empty, preventing harmonization error
          return(NULL)
      }

      handleFidPrefix(dt1_proc, dt2_proc)
    })

    # --- Observer: Initial Overlap Check & Modal ---
    # Use an observer instead of reactive to trigger side effects (modal)
    observe({
       # Requires harmonized pair to be calculated (meaning both files loaded & processed)
       req(harmonized_pair())

       pair <- harmonized_pair()
       # Additional check if harmonization returned NULL (e.g., empty input)
       req(pair)

       dt1_harm <- pair$dt1
       dt2_harm <- pair$dt2

       min_len <- min(nrow(dt1_harm), nrow(dt2_harm))

       # Default to TRUE if min_len is 0 (avoid division by zero, let downstream handle empty)
       passes_check <- TRUE
       overlap_pct <- 0

       if (min_len > 0) {
           common_fids <- intersect(dt1_harm$fid, dt2_harm$fid)
           overlap_pct <- length(common_fids) / min_len
           passes_check <- overlap_pct >= MIN_OVERLAP_PCT
       }

       if (!passes_check) {
           # Show modal only if it's not already active
           if (!low_overlap_modal_active()) {
               # Get feature types for a more informative message
               type1 <- data1$feature_type()
               type2 <- data2$feature_type()
               mismatch_msg <- if (type1 != type2 && type1 != "features" && type2 != "features") {
                   paste0(" Detected potential feature type mismatch (", type1, " vs ", type2, ").")
               } else {
                   ""
               }

               showModal(modalDialog(
                   title = tagList(shiny::icon("exclamation-triangle", class="text-warning"), " Low Feature Overlap"),
                   paste0("Warning: Feature ID overlap between the two datasets is only ",
                          round(overlap_pct * 100, 1),
                          "%, which is below the required minimum of ", MIN_OVERLAP_PCT * 100,
                          "%.", mismatch_msg,
                          " Concordance results may be unreliable or misleading. Please check if the datasets use comparable feature identifiers (e.g., both genes or both transcripts)."),
                   footer = modalButton("Dismiss"),
                   easyClose = FALSE # User must click Dismiss
               ))
               low_overlap_modal_active(TRUE) # Set flag that modal is shown
           }
       } else {
           # If overlap is OK, remove the modal if it was previously shown
           if (low_overlap_modal_active()) {
               removeModal()
               low_overlap_modal_active(FALSE) # Reset flag
           }
           # Also remove the older notification warning if it exists
           removeNotification(id=ns("overlap_warn"))
       }
       # Note: This observer doesn't return a value.
       # Downstream reactives will depend on filtered_overlap_metrics directly.
    }) # End overlap check observer

    # --- Reactive: Calculate Metrics on Filtered Data ---
    filtered_overlap_metrics <- reactive({
        # Require filtered data and harmonized pair
        req(data1$filtered(), data2$filtered(), harmonized_pair())

        dt1_filt_orig <- data1$filtered()
        dt2_filt_orig <- data2$filtered()
        pair_harm <- harmonized_pair()
        req(pair_harm) # Ensure harmonization didn't return NULL

        # --- Harmonize FIDs in Filtered Data ---
        # Get original fids from the *processed* data corresponding to the filtered rows
        # This is safer than assuming processed() has the same row order if filtering happened

        # Create mapping tables: original fid -> harmonized fid
        map1 <- data.table(orig_fid = data1$processed()$fid, harm_fid = pair_harm$dt1$fid, key = "orig_fid")
        map2 <- data.table(orig_fid = data2$processed()$fid, harm_fid = pair_harm$dt2$fid, key = "orig_fid")

        # Create copies to modify
        dt1_filt_harm <- copy(dt1_filt_orig)
        dt2_filt_harm <- copy(dt2_filt_orig)

        # Set keys for efficient merging
        setkey(dt1_filt_harm, fid)
        setkey(dt2_filt_harm, fid)

        # Update fids using the map (join and update)
        dt1_filt_harm[map1, fid := i.harm_fid]
        dt2_filt_harm[map2, fid := i.harm_fid]
        # --------------------------------------

        # Calculate metrics using the filtered data with harmonized fids
        calculateOverlapMetrics(dt1_filt_harm, dt2_filt_harm)
    })

    # --- Output: Overlap Summary Display ---
    output$overlap_summary_display <- renderUI({
        # Require that metrics have been calculated
        metrics <- filtered_overlap_metrics()
        # Use req() or validate()/need() to handle cases where metrics aren't ready
        validate(
            need(metrics, "Load two datasets and adjust filters to see overlap metrics.")
        )

        HTML(paste0(
            "<b>", formatCount(metrics$overlap_n), "</b> overlapping features in selection ",
            "(overlap coeff: ", metrics$overlap_coef, ")", "<br>",
            "Spearman rank correlation: <b>", ifelse(is.na(metrics$spearman_cor), "NA", metrics$spearman_cor), "</b> | ",
            "Weighted: ", ifelse(is.na(metrics$weighted_cor), "NA", metrics$weighted_cor)
       ))
    })
    # --- Plot Rendering (Placeholders for now) ---
    # These will eventually use filtered_overlap_metrics()$merged_data or similar
    # output$corrPlot <- renderPlotly({ ... })
    # output$catRankPlot <- renderPlotly({ ... })
    # output$catPvalPlot <- renderPlotly({ ... })

  })
}