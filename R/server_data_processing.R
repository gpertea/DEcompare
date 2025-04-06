# R/server_data_processing.R

library(shiny)
# library(data.table) # No longer explicitly needed here

#' Data Processing Server Module
#'
#' Handles gene symbol prefixing, initial overlap checks, and filtering of DE results.
#' Works with data.frame inputs/outputs.
#'
#' @param id Module ID.
#' @param deres1_server_out Reactive outputs from the first deres_input_server module.
#' @param deres2_server_out Reactive outputs from the second deres_input_server module.
#' @param find_sym_col_func Function like utils::find_sym_col.
#' @param add_gene_symbol_prefix_func Function like utils::add_gene_symbol_prefix.
#' @param check_initial_overlap_func Function like utils::check_initial_overlap.
#' @param filter_deres_func Function for filtering (e.g., utils::filter_deres).
#'
#' @return A reactiveValues object containing:
#'         - filtered_df1: Reactive filtered data.frame for dataset 1.
#'         - filtered_df2: Reactive filtered data.frame for dataset 2.
#'         - label1: Reactive label for dataset 1.
#'         - label2: Reactive label for dataset 2.
data_processing_server <- function(id, deres1_server_out, deres2_server_out,
                                   find_sym_col_func, add_gene_symbol_prefix_func,
                                   check_initial_overlap_func, filter_deres_func) {
  moduleServer(id, function(input, output, session) {

    rv_processed <- reactiveValues(
      processed_df1 = NULL, # Store the potentially prefixed df
      processed_df2 = NULL,
      raw_df1_used = NULL, # Track raw data used for last processing run
      raw_df2_used = NULL,
      processing_done = FALSE # Flag to track if prefixing/overlap check was done for current pair
    )

    # --- Observe Raw Data for Prefixing and Overlap Check ---
    observe({
      raw1 <- deres1_server_out$raw_data()
      raw2 <- deres2_server_out$raw_data()

      # Reset processed data if either input is NULL (e.g., file unloaded)
      if (!is.data.frame(raw1) || !is.data.frame(raw2)) {
          if (rv_processed$processing_done) { # Only reset if we previously processed something
              message("Resetting processed data as one input is missing.")
              rv_processed$processed_df1 <- NULL
              rv_processed$processed_df2 <- NULL
              rv_processed$raw_df1_used <- NULL
              rv_processed$raw_df2_used <- NULL
              rv_processed$processing_done <- FALSE
          }
          return()
      }

      # Check if processing is needed (data changed or first time both loaded)
      needs_processing <- !rv_processed$processing_done ||
                          !identical(raw1, isolate(rv_processed$raw_df1_used)) ||
                          !identical(raw2, isolate(rv_processed$raw_df2_used))

      if (needs_processing) {
          message("Processing raw data: Applying prefixes (if applicable) and checking overlap...")

          # Store the raw data we are processing this time
          # Use isolate to prevent this observer from triggering itself via these assignments
          isolate({
              rv_processed$raw_df1_used <- raw1
              rv_processed$raw_df2_used <- raw2
          })

          # Work on copies (base R data.frames are copied by default on modification)
          df1_proc <- raw1
          df2_proc <- raw2

          # --- Conditional Prefixing ---
          sym_col1 <- find_sym_col_func(df1_proc)
          sym_col2 <- find_sym_col_func(df2_proc)
          prefix_applied_msg <- NULL
          prefix_error <- FALSE

          if (!is.null(sym_col1) && !is.null(sym_col2)) {
              message("Symbol columns found in both datasets. Attempting prefixing.")
              # Pass copies to be safe, though the function should handle it
              prefix_result <- add_gene_symbol_prefix_func(df1_proc, df2_proc, sym_col1, sym_col2)
              df1_proc <- prefix_result$df1 # Get potentially modified df
              df2_proc <- prefix_result$df2
              prefix_error <- prefix_result$error

              if (prefix_result$applied) {
                   prefix_applied_msg <- "Applied gene symbol prefixes (SYMBOL|) to rownames based on 'fid'."
              } else if (prefix_result$error) {
                   prefix_applied_msg <- "Gene symbol prefixing failed (e.g., duplicates introduced). Using 'fid' as rownames."
              } else {
                   prefix_applied_msg <- "Gene symbol columns found, but no changes made to rownames (e.g., already prefixed or no symbols to add)."
              }
              showNotification(prefix_applied_msg, type = if(prefix_error) "warning" else "message", duration = 7)

          } else {
               message("Gene symbol columns ('", paste(SYMBOL_COLS, collapse="' or '"), "') not found in both datasets. Using original 'fid' as rownames.")
               # Ensure rownames are set from 'fid' if not already (should be done in load_deres_file, but double-check)
               if (!identical(rownames(df1_proc), df1_proc$fid)) rownames(df1_proc) <- df1_proc$fid
               if (!identical(rownames(df2_proc), df2_proc$fid)) rownames(df2_proc) <- df2_proc$fid
          }

          # Store the final processed dataframes (rownames are now the primary ID)
          rv_processed$processed_df1 <- df1_proc
          rv_processed$processed_df2 <- df2_proc

          # --- Initial Overlap Check ---
          # Perform check on the final rownames of the processed data
          overlap_check_result <- check_initial_overlap_func(df1_proc, df2_proc) # Use the processed dfs
          if (!is.null(overlap_check_result$warning_message)) {
              showNotification(overlap_check_result$warning_message, type = "warning", duration = 10)
          } else {
               msg <- sprintf("Initial overlap check passed: %d features (%.1f%%)",
                           overlap_check_result$overlap_count,
                           overlap_check_result$overlap_percent * 100)
               showNotification(msg, type = "message", duration = 5)
          }

          rv_processed$processing_done <- TRUE # Mark processing complete for this data pair
      }
    }) # End observe

    # --- Reactive Filtering (using processed data) ---
    # This runs whenever filter settings change OR the processed data changes
    filtered_df1_re <- reactive({
      message("--> Evaluating filtered_df1_re") # DEBUG START
      # Requires processed data to be available
      req(rv_processed$processed_df1)
      df_to_filter <- rv_processed$processed_df1
      message("   filtered_df1_re: Got processed_df1 (", nrow(df_to_filter), " rows)") # DEBUG

      settings <- deres1_server_out$filter_settings()
      # Ensure settings are valid numbers before filtering
      req(settings, is.numeric(settings$max_pval), is.numeric(settings$top_n))
      message("   filtered_df1_re: Got valid filter settings (P<=", settings$max_pval, ", N=", settings$top_n, ")") # DEBUG

      # Apply filtering using the utility function (expects/returns data.frame)
      filtered_result <- tryCatch({
          filter_deres_func(df_to_filter, settings$max_pval, settings$top_n)
      }, error = function(e) {
          message("!!! ERROR in filter_deres_func for df1: ", e$message) # DEBUG ERROR
          NULL # Return NULL on error
      })

      if(is.data.frame(filtered_result)) {
          message("<-- filtered_df1_re finished. Returning df with ", nrow(filtered_result), " rows.") # DEBUG END
      } else {
          message("<-- filtered_df1_re finished. Returning NULL due to error.") # DEBUG END
      }
      return(filtered_result)
    })

    filtered_df2_re <- reactive({
      message("--> Evaluating filtered_df2_re") # DEBUG START
      req(rv_processed$processed_df2)
      df_to_filter <- rv_processed$processed_df2
       message("   filtered_df2_re: Got processed_df2 (", nrow(df_to_filter), " rows)") # DEBUG

      settings <- deres2_server_out$filter_settings()
      req(settings, is.numeric(settings$max_pval), is.numeric(settings$top_n))
      message("   filtered_df2_re: Got valid filter settings (P<=", settings$max_pval, ", N=", settings$top_n, ")") # DEBUG

      filtered_result <- tryCatch({
          filter_deres_func(df_to_filter, settings$max_pval, settings$top_n)
      }, error = function(e) {
          message("!!! ERROR in filter_deres_func for df2: ", e$message) # DEBUG ERROR
          NULL
      })

      if(is.data.frame(filtered_result)) {
          message("<-- filtered_df2_re finished. Returning df with ", nrow(filtered_result), " rows.") # DEBUG END
      } else {
           message("<-- filtered_df2_re finished. Returning NULL due to error.") # DEBUG END
      }
      return(filtered_result)
    })

    # --- Return reactiveValues containing results ---
    results <- reactiveValues()
    observe({
        message("--> Updating results reactiveValues in data_processing_server") # DEBUG
        fdf1 <- filtered_df1_re()
        fdf2 <- filtered_df2_re()
        lab1 <- deres1_server_out$label()
        lab2 <- deres2_server_out$label()

        results$filtered_df1 <- fdf1
        results$filtered_df2 <- fdf2
        results$label1 <- lab1
        results$label2 <- lab2
        message("<-- Updated results: df1 valid=", is.data.frame(fdf1), ", df2 valid=", is.data.frame(fdf2)) # DEBUG
    })

    return(results)
  })
}
