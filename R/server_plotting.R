# R/server_plotting.R

library(shiny)
library(plotly)
library(ggplot2)
library(htmltools)

#' Plotting Server Module
#'
#' Renders plots and summary text based on processed data and overlap metrics.
#' Expects data.frame inputs.
#'
#' @param id Module ID.
#' @param processed_data_re A reactiveValues object from data_processing_server
#'                          (contains filtered_df1, filtered_df2, label1, label2 as data.frames/reactives).
#' @param overlap_results_re A reactiveValues object from overlap_metrics_server
#'                           (contains overlap_df, overlap_metrics, correlation_metrics as data.frame/reactives/lists).
#' @param plot_correlation_func Function like plot_correlation (needs to accept data.frames).
#' @param plot_cat_rank_func Function like plot_cat_rank.
#' @param plot_cat_pval_func Function like plot_cat_pval.
#' @param calculate_rank_diff_color_func Function like utils::calculate_rank_diff_color.
#'
#' @return Nothing (assigns outputs directly).
plotting_server <- function(id, processed_data_re, overlap_results_re,
                            plot_correlation_func, plot_cat_rank_func, plot_cat_pval_func,
                            calculate_rank_diff_color_func) {
  moduleServer(id, function(input, output, session) {
     message("--- Initializing plotting_server (Simplified Debug Mode) ---") # DEBUG INIT

    # --- Render Correlation Plot ---
    output$corr_plot <- renderPlotly({
      # Use req() on the reactiveValues elements needed
      req(processed_data_re$filtered_df1, processed_data_re$filtered_df2,
          overlap_results_re$overlap_df,
          processed_data_re$label1, processed_data_re$label2)

      # --- Debugging ---
      fdf1 <- processed_data_re$filtered_df1
      fdf2 <- processed_data_re$filtered_df2
      ov_df <- overlap_results_re$overlap_df
      lab1 <- processed_data_re$label1
      lab2 <- processed_data_re$label2

      # Ensure the required inputs are valid data.frames before plotting
      # (req already did this, but double-check doesn't hurt)
      req(is.data.frame(fdf1), is.data.frame(fdf2), is.data.frame(ov_df))

      # Call the plotting function (which must now handle data.frames)
      plot_correlation_func(
          filtered_df1 = fdf1,
          filtered_df2 = fdf2,
          overlap_df = ov_df,
          label1 = lab1,
          label2 = lab2,
          calculate_rank_diff_color_func = calculate_rank_diff_color_func
      )
    }) # End renderPlotly

    # --- Render Correlation Summary Text ---
     # Use the namespaced ID "plots-corr_summary_text"
    output$corr_summary_text <- renderUI({
      # Use req() on the reactiveValues elements needed
      req(overlap_results_re$overlap_metrics, overlap_results_re$correlation_metrics)
      overlap_metrics <- overlap_results_re$overlap_metrics
      corr_metrics <- overlap_results_re$correlation_metrics

      # Ensure metrics are lists as expected
      req(is.list(overlap_metrics), is.list(corr_metrics))

      overlap_count_fmt <- format(overlap_metrics$count, big.mark = ",", scientific = FALSE)
      overlap_coeff_fmt <- sprintf("%.1f%%", overlap_metrics$coeff * 100)
      spearman_cor_fmt <- if (is.na(corr_metrics$spearman_cor)) "NA" else sprintf("%.3f", corr_metrics$spearman_cor)
      weighted_cor_fmt <- if (is.na(corr_metrics$weighted_cor)) "NA" else sprintf("%.3f", corr_metrics$weighted_cor)

      tags$div(class = "corr-summary-text",
          tags$strong(overlap_count_fmt), "overlapping features in selection",
          "(overlap coeff:", tags$strong(overlap_coeff_fmt), ")",
          tags$br(),
          "Spearman rank correlation:", tags$strong(spearman_cor_fmt),
          "| Weighted:", tags$strong(weighted_cor_fmt)
      )
    })

    # --- Render Placeholder Plots ---
    output$cat_rank_plot <- renderPlot({
      # Pass filtered data.frames to the placeholder function
      req(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
      req(is.data.frame(processed_data_re$filtered_df1), is.data.frame(processed_data_re$filtered_df2))
      plot_cat_rank_func(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
    })

    output$cat_pval_plot <- renderPlot({
      # Pass filtered data.frames to the placeholder function
      req(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
      req(is.data.frame(processed_data_re$filtered_df1), is.data.frame(processed_data_re$filtered_df2))
      plot_cat_pval_func(processed_data_re$filtered_df1, processed_data_re$filtered_df2)
    })

  }) # End moduleServer
} # End plotting_server
