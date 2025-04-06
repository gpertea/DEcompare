# R/ui_data_loader.R

library(shiny)
library(bslib)

ui_data_loader <- function(id, title = "DE results set", default_label = "DE-results-X") {
  ns <- NS(id)

  bslib::card(
    class = "mb-1 p-2 border-0", # Minimal card styling
    bslib::card_header(
      class = "p-1",
      # Use accordion-like behavior for collapsing
      tags$button(
        type = "button",
        class = "btn btn-light btn-sm w-100 text-start d-flex justify-content-between align-items-center",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", ns("collapse_card")),
        `aria-expanded` = "true",
        `aria-controls` = ns("collapse_card"),
        span(title),
        bsicons::bs_icon("chevron-up") # Icon for collapse state
      )
    ),
    div(
      class = "collapse show", # Start expanded
      id = ns("collapse_card"),
      bslib::card_body(
        class = "p-1", # Reduce padding
        # Row 1: File Input & Filename
        div(class = "row g-1 align-items-center mb-1",
            div(class = "col-auto",
                # Minimal file input - styling needed via CSS
                fileInput(ns("file_in"), label = NULL, buttonLabel = "Browse..", placeholder = "", width = "100px")
            ),
            div(class = "col", # Takes remaining space
                span(class="text-muted small file-name-display", # Class for CSS targeting
                   textOutput(ns("file_name_display"), inline = TRUE)
                )
            )
        ),
        # Row 2: Label, Features, FDR
        div(class = "row g-1 align-items-center mb-1 data-loader-label-row", # Class for width calculation
            div(class = "col-auto",
                tags$label("Label", `for` = ns("label_in"), class="form-label mb-0")
            ),
            div(class = "col-auto", # Fixed width for input
                style = "width: 150px;",
                textInput(ns("label_in"), label = NULL, value = default_label, width = "100%", placeholder = default_label) |>
                  tagAppendAttributes(maxlength = MAX_LABEL_LEN)
            ),
            div(class = "col-auto",
                # Placeholder for feature count
                htmlOutput(ns("feature_count_display"), inline = TRUE)
            ),
            div(class = "col-auto",
                # Placeholder for FDR count
                htmlOutput(ns("fdr_count_display"), inline = TRUE)
            )
        ),
        # Row 3: Max P value slider and input
        div(class = "row g-1 align-items-center mb-1",
            div(class = "col-auto",
                tags$label("Max P value", `for` = ns("max_p_val_num"), class="form-label mb-0")
            ),
            div(class = "col-auto", style = "width: 60px;",
                numericInput(ns("max_p_val_num"), label = NULL, value = 1.00, min = 0, max = 1, step = 0.01, width = "100%")
            ),
            div(class = "col", # Slider takes remaining space
                sliderInput(ns("max_p_val_sli"), label = NULL, min = 0, max = 1, value = 1.00, step = 0.01, width = "100%")
            )
        ),
        # Row 4: Top N selection
        div(class = "row g-1 align-items-center mb-1",
            div(class = "col-auto",
                tags$label("Top N selection", `for` = ns("top_n_in"), class="form-label mb-0")
            ),
            div(class = "col-auto", style = "width: 80px;",
                numericInput(ns("top_n_in"), label = NULL, value = 0, min = 0, max = MAX_TOP_N, step = 1, width = "100%")
            )
        ),
        # Row 5: Selected features summary
        div(class = "row g-1 align-items-center",
             div(class = "col",
                 htmlOutput(ns("selected_count_display"), inline = TRUE)
             )
        )
      ) # end card_body
    ) # end collapse div
  ) # end card
}
