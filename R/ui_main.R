# R/ui_main.R

library(shiny)
library(bslib)
library(sass)


custom_css <- "
.smaller-row {
    /* color: red !important; */
    --bs-gutter-x: 0.2rem !important;
  }

  .smaller-row .form-group, .smaller-row .input-group, .smaller-row .form-control,
     .smaller-row .shiny-input-container, .smaller-row .form-label {
    /* font-size: 0.8rem !important;
    margin-bottom: 0 !important;
    margin-top: 0 !important;
    padding-bottom: 2px !important;
    padding-top: 2px !important;
    line-height: 0.8rem !important;
    color: red !important; */
    padding: 2px 2px !important;
    margin-bottom: 0 !important;
    --bs-gutter-y: -0.6rem !important;
  }
" ##

my_theme <- bslib::bs_theme(version = 5, bootswatch = "cosmo", base_font = font_google("Inter", local = FALSE), font_scale=0.8) |>
  bs_add_rules(custom_css)

ui_main <- function() {
  page_sidebar(
    title = NULL, # No main title
    theme = my_theme,

    sidebar = sidebar(
      id = "dls_sidebar",
      title = NULL, # No sidebar title
      width = DLS_SIDEBAR_WIDTH, # Use configured width
      open = "always", # Keep sidebar open
      class = "p-1", # Reduce padding


      # Include the two data loader UI modules
      ui_data_loader(id = "de1", title = "DE results set 1", default_label = DEFAULT_LABEL_1),
      ui_data_loader(id = "de2", title = "DE results set 2", default_label = DEFAULT_LABEL_2)

    ), # end sidebar

    # Main content area for CAVS
    ui_cavs(id = "cavs")

  ) # end page_sidebar
}
