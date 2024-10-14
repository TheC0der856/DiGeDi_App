distinct_genetic_diversity_table <- function(data, decimal_places) {
  # Ensure data is available
  req(data)
  # round decimal places
  rounded_data <- data %>%
    mutate_if(is.numeric, ~ round(., decimal_places))
  # create and return table
  datatable(
    rounded_data,
    extensions = 'Buttons',
    selection = 'single',
    options = list(
      dom = "Blfrtip",
      buttons = list("copy", list(
        extend = "collection",
        buttons = c("csv", "excel", "pdf"),
        text = "Download"
      )),
      lengthMenu = list(c(10, 20, -1), c(10, 20, "All")),
      pageLength = 10
    ),
    rownames = FALSE,
    escape = FALSE
  )
}