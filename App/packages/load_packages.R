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
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library("vegan")
library("adegenet")
library("shiny")
library("shinyBS")
library("DT")
library("dplyr")