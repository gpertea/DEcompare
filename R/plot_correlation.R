# R/plot_correlation.R (Updated)

library(plotly)
# library(data.table) # No longer needed

#' Generate Correlation Scatter Plot (Handles data.frame)
#'
#' Creates an interactive scatter plot using plot_ly to visualize the
#' correlation of t-statistics between two DE results sets.
#'
#' @param filtered_df1 Filtered data.frame for the first dataset. Used for axis range.
#' @param filtered_df2 Filtered data.frame for the second dataset. Used for axis range.
#' @param overlap_df Data.frame containing only the overlapping features,
#'                   with columns 't.x' and 't.y' and rownames set to feature IDs.
#' @param label1 Label for the x-axis (from dataset 1).
#' @param label2 Label for the y-axis (from dataset 2).
#' @param calculate_rank_diff_color_func Function to calculate point colors.
#'
#' @return A plotly object.
#' @export
plot_correlation <- function(filtered_df1, filtered_df2, overlap_df, label1, label2, calculate_rank_diff_color_func) {

  # --- Input Validation ---
  if (!is.data.frame(overlap_df) || nrow(overlap_df) == 0 ||
      !all(c("t.x", "t.y") %in% names(overlap_df))) {
    # Return an empty plot with a clear message if no valid overlap data
    message("plot_correlation: No valid overlapping features to plot.")
    return(
        plot_ly() %>%
        layout(
            title = "No overlapping features in current selection",
            xaxis = list(title = label1, range=c(-1,1), showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
            yaxis = list(title = label2, range=c(-1,1), showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
            margin = list(t = 50) # Add margin for title
        )
    )
  }
   message("plot_correlation: Plotting ", nrow(overlap_df), " overlapping features.")

   # --- Determine Axis Ranges ---
   # Use the filtered data frames if valid, otherwise fallback to overlap data
   get_range <- function(df, col = "t") {
       if (is.data.frame(df) && nrow(df) > 0 && col %in% names(df)) {
           r <- range(df[[col]], na.rm = TRUE)
           # Add padding if range is valid
           if(all(is.finite(r))) {
               padding <- diff(r) * 0.05
               # Handle case where range is zero
               if (padding == 0) padding <- 1
               return(r + c(-padding, padding))
           }
       }
       return(NULL) # Return NULL if range cannot be determined
   }

   range_x <- get_range(filtered_df1, "t")
   range_y <- get_range(filtered_df2, "t")

   # Fallback to overlap data range if filtered data range failed
   if (is.null(range_x)) range_x <- get_range(overlap_df, "t.x")
   if (is.null(range_y)) range_y <- get_range(overlap_df, "t.y")

   # Final fallback if all else fails
   if (is.null(range_x)) range_x <- c(-5, 5)
   if (is.null(range_y)) range_y <- c(-5, 5)

   message("plot_correlation: X-axis range: ", paste(round(range_x, 2), collapse=","))
   message("plot_correlation: Y-axis range: ", paste(round(range_y, 2), collapse=","))

  # --- Calculate Colors ---
  point_colors <- calculate_rank_diff_color_func(overlap_df$t.x, overlap_df$t.y)
  if(length(point_colors) != nrow(overlap_df)) {
      warning("plot_correlation: Color vector length mismatch. Using grey.")
      point_colors <- "#808080" # Fallback color
  }

  # --- Create Hover Text ---
  # Use rownames from the overlap_df
  hover_texts <- rownames(overlap_df)
  if(is.null(hover_texts) || length(hover_texts) != nrow(overlap_df)) {
      hover_texts <- paste("Feature", 1:nrow(overlap_df)) # Fallback hover text
  }

  # --- Generate Plot ---
  p <- tryCatch({
      plot_ly(
            # Explicitly pass data frame
            data = overlap_df,
            x = ~t.x, # Use formula notation with data frame
            y = ~t.y,
            type = "scattergl",
            mode = "markers",
            marker = list(
                color = point_colors,
                size = 5,
                opacity = 0.7
                # Consider adding line for border: line = list(width = 0.5, color = 'grey')
            ),
            # Use key for potential cross-filtering later (optional)
            # key = ~rownames(overlap_df),
            text = hover_texts, # Pass the generated hover texts
            hoverinfo = "text+x+y", # Specify what info to show on hover
            source = "corr_plot"
        ) %>%
        layout(
            xaxis = list(title = label1, range = range_x, zeroline=TRUE, zerolinecolor='#cccccc', zerolinewidth=1),
            yaxis = list(title = label2, range = range_y, zeroline=TRUE, zerolinecolor='#cccccc', zerolinewidth=1),
            showlegend = FALSE,
            dragmode = "pan"
        ) %>%
        config(
            scrollZoom = TRUE,
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian")
        )
    }, error = function(e) {
        message("Error during plot_ly generation: ", e$message)
        # Return the "no overlap" plot as an error fallback
         return(
            plot_ly() %>%
            layout(
                title = "Error generating plot",
                xaxis = list(title = label1, range=c(-1,1), showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                yaxis = list(title = label2, range=c(-1,1), showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                margin = list(t = 50)
            )
        )
    })


  return(p)
}
