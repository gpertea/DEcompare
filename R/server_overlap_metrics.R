# R/server_overlap_metrics.R

library(shiny)
# library(data.table) # No longer needed here

#' Overlap and Metrics Server Module
#'
#' Calculates overlapping features and associated metrics (overlap, correlation).
#' Works with data.frame inputs/outputs.
#'
#' @param id Module ID.
#' @param processed_data_re A reactiveValues object from data_processing_server,
#'                          containing filtered_df1, filtered_df2 (as data.frames).
#' @param calculate_overlap_metrics_func Function like utils::calculate_overlap_metrics.
#' @param calculate_correlation_metrics_func Function like utils::calculate_correlation_metrics.
#'
#' @return A reactiveValues object containing:
#'         - overlap_df: Reactive data.frame of overlapping features with t.x, t.y.
#'         - overlap_metrics: Reactive list with overlap count and coefficient.
#'         - correlation_metrics: Reactive list with spearman and weighted correlation.
overlap_metrics_server <- function(id, processed_data_re, calculate_overlap_metrics_func, calculate_correlation_metrics_func) {
  moduleServer(id, function(input, output, session) {
    message("--- Initializing overlap_metrics_server ---") # DEBUG INIT

    # --- Reactive Overlap Calculation ---
    overlap_re <- reactive({
      message("--> Evaluating overlap_re") # DEBUG START
      # Use req() on the reactiveValues elements which should be data.frames
      # Check the input reactive directly
      req(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
      fdf1 <- processed_data_re$filtered_df1
      fdf2 <- processed_data_re$filtered_df2
      message("   overlap_re: Received filtered_df1 (", nrow(fdf1), " rows), filtered_df2 (", nrow(fdf2), " rows)") # DEBUG

      # Ensure they are valid data.frames before proceeding
      req(is.data.frame(fdf1), is.data.frame(fdf2))

      common_features <- intersect(rownames(fdf1), rownames(fdf2))
      message("   overlap_re: Found ", length(common_features), " common features.") # DEBUG

      if (length(common_features) == 0) {
          message("<-- overlap_re finished. Returning empty df (no overlap).") # DEBUG END
          # Return an empty data.frame with expected columns
          return(data.frame(t.x = numeric(0), t.y = numeric(0)))
      }

      # Subset and merge
      merged_df <- tryCatch({
          fdf1_sub <- fdf1[common_features, "t", drop = FALSE]
          fdf2_sub <- fdf2[common_features, "t", drop = FALSE]
          colnames(fdf1_sub) <- "t.x"
          colnames(fdf2_sub) <- "t.y"
          fdf1_sub$feature_id_for_merge <- rownames(fdf1_sub)
          fdf2_sub$feature_id_for_merge <- rownames(fdf2_sub)
          mdf <- merge(fdf1_sub, fdf2_sub, by = "feature_id_for_merge", all = FALSE)
          rownames(mdf) <- mdf$feature_id_for_merge
          mdf$feature_id_for_merge <- NULL
          req("t.x" %in% names(mdf), "t.y" %in% names(mdf)) # Final check
          mdf
      }, error = function(e) {
          message("!!! ERROR during overlap merge: ", e$message) # DEBUG ERROR
          NULL # Return NULL on error
      })

      if(is.data.frame(merged_df)) {
          message("<-- overlap_re finished. Returning merged df with ", nrow(merged_df), " rows.") # DEBUG END
      } else {
           message("<-- overlap_re finished. Returning NULL due to error.") # DEBUG END
      }
      return(merged_df)
    })

    # --- Reactive Metrics Calculation ---
    overlap_metrics_re <- reactive({
        message("--> Evaluating overlap_metrics_re") # DEBUG
        req(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
        metrics <- calculate_overlap_metrics_func(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
        message("<-- overlap_metrics_re result: count=", metrics$count) # DEBUG
        return(metrics)
    })

    correlation_metrics_re <- reactive({
        message("--> Evaluating correlation_metrics_re") # DEBUG
        overlap_data <- overlap_re()
        req(overlap_data) # Requires the overlap calculation to succeed
        metrics <- calculate_correlation_metrics_func(overlap_data)
        message("<-- correlation_metrics_re result: cor=", round(metrics$spearman_cor, 3)) # DEBUG
        return(metrics)
    })

    # --- Return reactiveValues containing results ---
    results <- reactiveValues()
    observe({
        message("--> Updating results reactiveValues in overlap_metrics_server") # DEBUG
        ov_df <- overlap_re()
        ov_met <- overlap_metrics_re()
        co_met <- correlation_metrics_re()

        results$overlap_df <- ov_df
        results$overlap_metrics <- ov_met
        results$correlation_metrics <- co_met
        message("<-- Updated results: overlap_df valid=", is.data.frame(ov_df), ", overlap_metrics valid=", is.list(ov_met), ", corr_metrics valid=", is.list(co_met)) # DEBUG
    })

    return(results)
  })
}