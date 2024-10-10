identify_kba <- function(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, button_name, criterion_name) {
  observeEvent(input[[button_name]], {
    
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA" 
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info[[criterion_name]])) == 0) {
        "None of the areas qualifies as KBA."
      } else {
        paste0("Following areas qualify as KBAs: ", paste(names(reactive_KBA_info[[criterion_name]]), collapse = ", "), ".")
      }
    })
    
    output$DGD_table <- renderTable({
      NULL  
    })
  })
}