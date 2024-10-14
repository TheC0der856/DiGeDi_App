reset_results <- function(reactive_DGD_table, reactive_KBA_info, output) {
  reactive_DGD_table$df_ratios <- NULL
  reactive_KBA_info$A1a <- NULL
  reactive_KBA_info$A1b <- NULL
  reactive_KBA_info$A1c <- NULL
  reactive_KBA_info$A1d <- NULL
  reactive_KBA_info$B1  <- NULL
  output$KBA_identif <- NULL
}