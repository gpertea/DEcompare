# R/srv_main.R

library(shiny)

srv_main <- function(input, output, session) {

  # Call data loader server modules
  de1_res <- srv_data_loader(id = "de1")
  de2_res <- srv_data_loader(id = "de2")

  # Call CAVS server module (passing dummy reactives for now)
  srv_cavs(id = "cavs", data1 = de1_res, data2 = de2_res)

}
