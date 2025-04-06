# R/ui_cavs.R

library(shiny)
library(bslib)
library(plotly)

ui_cavs <- function(id) {
  ns <- NS(id)

  div(
    style = paste0("min-width:", MIN_CAVS_WIDTH, "; min-height:", MIN_CAVS_HEIGHT, "; height: calc(100vh - 50px); display: flex; flex-direction: column;"), # Adjust height calc if needed

    # Tabset Panel (will hold plots later)
    bslib::navset_tab(
      id = ns("cavs_tabs"),
      bslib::nav_panel("Correlation",
         div(style="flex-grow: 1; min-height: 100px;", # Placeholder for plot area
             h4("Correlation Plot Area (placeholder)")
             # plotlyOutput(ns("corrPlot"), height = "100%") # Add later
         )
      ),
      bslib::nav_panel("CAT-rank",
         div(style="flex-grow: 1; min-height: 100px;", # Placeholder for plot area
             h4("CAT-rank Plot Area (placeholder)")
             # plotlyOutput(ns("catRankPlot"), height = "100%") # Add later
         )
      ),
      bslib::nav_panel("CAT-p-val",
         div(style="flex-grow: 1; min-height: 100px;", # Placeholder for plot area
             h4("CAT-p-val Plot Area (placeholder)")
             # plotlyOutput(ns("catPvalPlot"), height = "100%") # Add later
         )
      )
    ) |> tagAppendAttributes(style = "flex-grow: 1; display: flex; flex-direction: column;"), # Make tabset grow

    # Fixed Summary Section at the bottom
    div(
      class = "cavs-summary-section p-2 mt-auto", # Class for styling, mt-auto pushes down if space allows
      style = "flex-shrink: 0; border-top: 1px solid #ddd;", # Prevent shrinking, add border
      htmlOutput(ns("overlap_summary_display"), inline = FALSE)
    )
  )
}
