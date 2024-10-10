upload_example_files <- function(filename) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      file.copy(paste("example_data_sets/", filename, sep = ""), file)    
    }
  )
}

upload_example_csv <- function(data) {
  downloadHandler(
    filename = function() {
      "nancycats.csv"
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
}