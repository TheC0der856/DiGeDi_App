direct_file_upload <- function(file_ext, output) {
  if (file_ext %in% c("str", "stru")) {
    output$str_ui <- renderUI({
      tagList(
        numericInput("n_ind", label = h3("How many genotypes are there?"), value = "xxx", min = 2),
        numericInput("n_loc", label = h3("How many markers are there?"), value = "xxx", min = 1),
        numericInput("col_geno", label = h3("Which column contains labels for genotypes ('0' if absent)?"), value = "xxx", min = 0),
        numericInput("col_area", label = h3("Which column contains the site names?"), value = "xxx", min = 1)
      )
    })
  } else if (file_ext %in% c("gen", "dat", "gtx", "csv")) {
    output$str_ui <- renderUI({})
  } else if (file_ext == "structure") {
    output$str_ui <- renderUI({})
    showNotification("Invalid file format. Please upload a .str or .stru file.", type = "error")
    return(NULL)
  } else {
    output$str_ui <- renderUI({})
    showNotification("Invalid file format. Please upload a CSV, GENETIX (.gtx), Genpop (.gen), Fstat (.dat) or STRUCTURE (.str or .stru) file.", type = "error")
    return(NULL)
  }
}