library(shiny)
library(bslib)


customFileInput <- function(inputId, label, multiple = FALSE, accept = NULL, buttonLabel = "Browse",
                           placeholder = "No file selected", showProgress = FALSE) {

  input <- fileInput(inputId, label, multiple, accept, buttonLabel=buttonLabel, placeholder=placeholder)
  if (!showProgress) {
    input <- htmltools::tagQuery(input)$
      find(".shiny-file-input-progress")$
      remove()$
      allTags()
  }

  return(input)
}

ui_data_loader <- function(id, title = "DE results set", default_label = "DE-results-X") {
  ns <- NS(id)

  div( class = "mb-2 p-2 border rounded shadow-sm",
      # Row 1: Label, Features, FDR (No margin bottom)
        div(class = "row g-0 align-items-center data-loader-label-row smaller-row", # Removed mb-1
            div(class = "col-auto",
                tags$label("Label", `for` = ns("label_in"), class="form-label") # Removed mb-0 here, control via CSS
            ),
            div(class = "col-auto",
                style = "width: 150px;",
                textInput(ns("label_in"), label = NULL, value = default_label, width = "100%", placeholder = default_label) |>
                  tagAppendAttributes(maxlength = MAX_LABEL_LEN)
            ),
            div(class = "col-auto mx-3", style='color: #606060;',
                htmlOutput(ns("feature_count_display"), inline = TRUE)
            ),
            div(class = "col-auto", style='color: #606060;',
                htmlOutput(ns("fdr_count_display"), inline = TRUE)
            )
        ),
        # Row 2: File Input & Filename (No margin bottom: mb-0 or remove mb-*)
        div(class = "row g-0 align-items-center smaller-row", # Removed mb-1
            div(class = "col-12",
                #customFileInput(ns("file_in"), label = NULL)
                # Replace direct call with uiOutput
                uiOutput(ns("file_input_ui"))
            ) #,
            # div(class = "col",
            #    span(class="text-muted small file-name-display",
            #       textOutput(ns("file_name_display"), inline = TRUE)
            #    )
            #)
        ),
        # Row 3: Max P value slider and input (No margin bottom, adjusted cols)
        div(class = "row g-0 align-items-center smaller-row", # Removed mb-1
            div(class = "col-auto pe-0",
                tags$label("Max P value", `for` = ns("max_p_val_num"), class="form-label") # Removed mb-0
            ),
            # Increased width for numeric input's column
            div(class = "col-2 px-0", # Use Bootstrap col for better control, remove padding-end
                numericInput(ns("max_p_val_num"), label = NULL, value = 1.00, min = 0, max = 1, step = 0.01, width = "100%")
            ),
            # Slider takes remaining space in its column
            div(class = "col ps-1", # Add padding-start to separate from numeric
                sliderInput(ns("max_p_val_sli"), label = NULL, min = 0, max = 1, value = 1.00, step = 0.01, width = "100%")
            )
        ),
        # Row 4: Top N selection and
        div(class = "row g-0 align-items-center smaller-row pt-2", # Removed mb-1
            div(class = "col-auto pe-0",
                tags$label("Top N selection", `for` = ns("top_n_in"), class="form-label") # Removed mb-0
            ),
            div(class = "col-auto pb-0", style = "width: 4.1rem;",
                numericInput(ns("top_n_in"), label = NULL, value = 0, min = 0, max = MAX_TOP_N, step = 1, width = "100%")
            ),
            div(class = "col-auto ms-auto", style='color: #2c327d; font-size: 1rem; border: 1px solid #6c72CD; border-radius: 3px; padding: 2px 5px;',
                 htmlOutput(ns("selected_count_display"), inline = TRUE)
             )

        )

    ) # end card_body
}
