load_scripts_from_directory <- function(dir) {
  r_files <- list.files(path = dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (file in r_files) {
    source(file)
  }
}
