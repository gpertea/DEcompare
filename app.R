# app.R

# --- Libraries ---
library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(qs)
library(htmltools)
library(ggplot2) # Needed for placeholder plots

# --- Source Modules and Utilities ---
source("R/utils.R")
source("R/ui_module_deres.R")
# Source Plotting Functions (used by plotting module)
source("R/plot_correlation.R")
source("R/plot_cat_rank.R")
source("R/plot_cat_pval.R")
# Source Server Modules
source("R/server_data_processing.R")
source("R/server_overlap_metrics.R")
source("R/server_plotting.R")


# --- Global Options ---
options(shiny.maxRequestSize = 100*1024^2)

# --- UI Definition ---
# (UI definition remains unchanged from the previous version)
ui <- page_sidebar(
  title = "DE Results Concordance Explorer",
  # theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  # Try a different theme or customize font sizes
  theme = bslib::bs_theme(
      version = 5,
      preset = "cosmo", # Try cosmo or journal or default bootstrap
      # Or explicitly set fonts/sizes
      # base_font = bslib::font_google("Inter", local = FALSE),
      # font_scale = 0.9, # Scale all fonts down
      # "font-size-base" = "0.9rem", # Directly set base font size variable
      # heading_font = bslib::font_google("Inter", local=FALSE),
      # "headings-font-weight" = "500"
      ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
sidebar = sidebar(
    title = "Data Loading & Filtering",
    width = "600px", # <--- Explicitly set sidebar width here
    # Module UI calls remain the same
    deres_input_ui("deres-1", label_default = "DE-results-1"),
    deres_input_ui("deres-2", label_default = "DE-results-2")
  ), # End sidebar
  ## remember to use the plots- prefix for the plot outputs (the namespace set by plotting_server)
  navset_tab(
    id = "main_tabs",
    nav_panel(
      title = "Correlation",
      value = "tab_corr",
      card(
        full_screen = TRUE,
        card_body(fillable = TRUE, fill = TRUE, plotlyOutput("plots-corr_plot", height = "100%"))
      ),
      uiOutput("plots-corr_summary_text")
    ),
    nav_panel(
      title = "CAT-rank",
      value = "tab_cat_rank",
      plotOutput("plots-cat_rank_plot")
    ),
    nav_panel(
      title = "CAT-p-val",
      value = "tab_cat_pval",
      plotOutput("plots-cat_pval_plot")
    )
  )
)

# --- Server Logic (Modularized) ---
server <- function(input, output, session) {

  # --- Instantiate Input Modules ---
  # Pass the necessary utility functions from utils.R
  deres1_server_out <- deres_input_server("deres-1",
                                      load_deres_file_func = load_deres_file,
                                      filter_deres_func = filter_deres) # filter_deres needed for internal UI update
  deres2_server_out <- deres_input_server("deres-2",
                                      load_deres_file_func = load_deres_file,
                                      filter_deres_func = filter_deres)

  # --- Instantiate Data Processing Module ---
  # Pass the outputs of the input modules and necessary util functions
  processed_data_re <- data_processing_server("data_proc",
                                             deres1_server_out = deres1_server_out,
                                             deres2_server_out = deres2_server_out,
                                             find_sym_col_func = find_sym_col,
                                             add_gene_symbol_prefix_func = add_gene_symbol_prefix,
                                             check_initial_overlap_func = check_initial_overlap,
                                             filter_deres_func = filter_deres)

  # --- Instantiate Overlap & Metrics Module ---
  # Pass the output of the data processing module and necessary util functions
  overlap_results_re <- overlap_metrics_server("overlap_metr",
                                              processed_data_re = processed_data_re,
                                              calculate_overlap_metrics_func = calculate_overlap_metrics,
                                              calculate_correlation_metrics_func = calculate_correlation_metrics)

  # --- Instantiate Plotting Module ---
  # Pass outputs from previous modules and necessary plotting/util functions
  ## this "plots" string will require the plots- prefix for the plot outputs (namespace)
  plotting_server("plots",
                  processed_data_re = processed_data_re,
                  overlap_results_re = overlap_results_re,
                  plot_correlation_func = plot_correlation,
                  plot_cat_rank_func = plot_cat_rank,
                  plot_cat_pval_func = plot_cat_pval,
                  calculate_rank_diff_color_func = calculate_rank_diff_color)

} # End server

# --- Run App ---
shinyApp(ui = ui, server = server)
