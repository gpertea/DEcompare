# R/ui_main.R

library(shiny)
library(bslib)

ui_main <- function() {
  page_sidebar(
    title = NULL, # No main title
    theme = bslib::bs_theme(version = 5, bootswatch = "cosmo", base_font = font_google("Inter", local = FALSE)), # Using a compact theme

    sidebar = sidebar(
      id = "dls_sidebar",
      title = NULL, # No sidebar title
      width = DLS_SIDEBAR_WIDTH, # Use configured width
      open = "always", # Keep sidebar open
      class = "p-1", # Reduce padding

      # Add custom CSS via www/styles.css
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      # Include the two data loader UI modules
      ui_data_loader(id = "de1", title = "DE results set 1", default_label = DEFAULT_LABEL_1),
      ui_data_loader(id = "de2", title = "DE results set 2", default_label = DEFAULT_LABEL_2)

    ), # end sidebar

    # Main content area for CAVS
    ui_cavs(id = "cavs")

  ) # end page_sidebar
}
