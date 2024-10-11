if (!requireNamespace("vegan", quietly = TRUE)) {      
  install.packages("vegan")
}
if (!requireNamespace("adegenet", quietly = TRUE)) {   
  install.packages("adegenet")
}
if (!requireNamespace("shiny", quietly = TRUE)) {   
  install.packages("shiny")
}
if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")
}

library("vegan")
library("adegenet")
library("shiny")
library("shinyBS")