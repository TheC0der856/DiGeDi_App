identify_kba <- function(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, button_name, criterion_name) {
  observeEvent(input[[button_name]], {
    
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "KBA" 
    
    output$KBA_identif <- renderText({
      if (length(names(reactive_KBA_info[[criterion_name]])) == 0) {
        "None of the sites qualify becoming a KBA."
      } else {
        paste0("Following sites qualify becoming a KBA: ", paste(names(reactive_KBA_info[[criterion_name]]), collapse = ", "), ".")
      }
    })
    output$DGD_table <- DT::renderDataTable({
      NULL  
    })
  })
}