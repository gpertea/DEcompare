# R/srv_main.R

library(shiny)

srv_main <- function(input, output, session) {

  # Call data loader server modules, passing the default labels
  de1_res <- srv_data_loader(id = "de1", default_label = DEFAULT_LABEL_1)
  de2_res <- srv_data_loader(id = "de2", default_label = DEFAULT_LABEL_2)

  # Call CAVS server module, passing the list of reactives from each loader
  srv_cavs(id = "cavs", data1 = de1_res, data2 = de2_res)

}
