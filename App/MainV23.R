# import Code for ...
# ... packages:
source("packages/load_packages.R")
# ... functions:
# (example files for download)
source("functions/upload_example_files.R")
source("functions/download_datasets.R")
source("functions/direct_file_upload.R")
# (for layout)
source("functions/UI_styles.R")
source("functions/UI_datasets.R")
source("functions/UI_threatened_options.R")
# (display main calculations)
source("functions/reset_results.R")
source("functions/calculate_distinct_genetic_diversity.R")
source("functions/identify_KBAs.R")


server <- function(input, output, session) {
  
  ############### file upload ###################
  # Upload example files for user:
  data <- read.csv("example_data_sets/df.csv")
  output$downloadData <- upload_example_csv(data)
  output$downloadGENETIX <- upload_example_files("Supp_Mat_1_Pperr_24loci_3pop.gtx")
  output$downloadGenpop <- upload_example_files("Microsatellite_dataset.gen")
  output$downloadFstat <- upload_example_files("138.dat")
  output$downloadSTRUCTURE <- upload_example_files("AllPetros122410.str")
  
  # Offer option to download example files:
  observeEvent(input$show_example_files, { # Show buttons for several files after "Download example files" button was pressed
    output$download_buttons <- renderUI({
      download_datasets() # display download buttons with info icons
    })
  })
  
  # Create reactive values to ...
  # ... clear previous outputs when a new file is uploaded
  reactive_DGD_table <- reactiveValues(df_ratios = NULL)
  reactive_KBA_info <- reactiveValues(A1a = NULL, A1b = NULL, A1c = NULL, A1d = NULL, B1 = NULL)
  # .... control which output is displayed (distinct genetic diversity or identified KBAs)
  reactive_output <- reactiveValues(display = NULL)
  
  # If a new file is uploaded by the user ...
  observeEvent(input$file, {
    req(input$file)
    # ... delete previous results.
    reset_results(reactive_DGD_table, reactive_KBA_info, output) 
    #  ... extract file ending to ask questions for STRUCTURE files and show error messages for wrong file formats.
    file_ext <- tools::file_ext(input$file$name)  
    direct_file_upload(file_ext, output) 
  })
  
  
  ################ distinct genetic diversity #####################
  # Display distinct genetic diversity
  observeEvent(input$display_genetic_diversity_btn, {
    req(input$file)
    calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
    reactive_output$display <- "table"  # display the table
    
    output$DGD_table <- renderTable({
      reactive_DGD_table$df_ratios
    }, rownames = FALSE, colnames = TRUE, sanitize.text.function = function(x) x)
    output$KBA_identif <- renderText({
      NULL  # Clear KBA identif output
    })
  })
  
  # Download distinct genetic diversity as a .csv
  output$download_DGD <- downloadHandler(
    filename = function() {
      "distinct_genetic_diversity.csv"
    },
    content = function(file) {
      calculate_DGD(input, reactive_DGD_table, reactive_KBA_info)
      # Change column names
      new_colnames <- c("Area", "Delta+j", "Delta+j_in_%")
      colnames(reactive_DGD_table$df_ratios) <- new_colnames
      # save file
      write.csv(reactive_DGD_table$df_ratios, file, row.names = FALSE)
    }
  )
  
  ################### identify KBAs ####################
  identify_kba(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, "A1a_button", "A1a")
  identify_kba(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, "A1b_button", "A1b")
  identify_kba(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, "A1c_button", "A1c")
  identify_kba(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, "A1d_button", "A1d")
  identify_kba(input, reactive_DGD_table, reactive_KBA_info, reactive_output, output, "restricted_btn", "B1")
  
  
  # Threatened species identification
  observeEvent(input$threatened_btn, {
    output$threatened_options <- renderUI({
      threatened_options_UI()
    })
    output$KBA_identif <- renderText({
      "Is your species Vulnerable or Critically Endangered/Endangered?"
    }) 
    
    output$DGD_table <- renderTable({
      NULL  # Clear DGD table output
    })
  })
  
  ################## Display results #####################
  # Display distinct genetic diversity table or identified KBAs
  output$main_content <- renderUI({
    if (is.null(reactive_output$display)) {
      NULL  
    } else if (reactive_output$display == "table") {
      tableOutput("DGD_table")
    } else if (reactive_output$display == "KBA") {
      textOutput("KBA_identif")
    } else {
      NULL  
    }
  })
  
  ################ Download Manual #####################
  output$download_manual <- downloadHandler(
    filename = function() {"Distinct_Genetic_Diversity_App_Manual.pdf"},
    content = function(file) {file.copy("Distinct_Genetic_Diversity_App_Manual.pdf", file)  
    }
  )
}

ui <- fluidPage(
  tags$head(
    UI_styles()
  ),
  
  div(class = "header",
      div(
        img(src = "Shiny_logo.png", class = "logo"),
        div(class = "title", "Distinct Genetic Diversity"),
      ),
      div(
        #(src = "iucn.png", height = "100px"),
        img(src = "uni.png", height = "50px"),
      )
  ),
  
  fluidRow(
    column(6, 
           # Custom header with info icon next to "File input"
           div(
             h3(
               "Upload your file", 
               span(id = "file_info", class = "info-icon", icon("info-circle"))  # Icon for popover
             )
           ),
  
           fileInput("file", label = NULL),  # No label here, as we added a custom one above
           bsPopover(id = "file_info", 
                     title = "Accepted Formats", 
                     content = "Accepted formats are: .csv, GENETIX (.gtx), Genpop (.gen), Fstat (.dat), or STRUCTURE (.str or .stru).", 
                     placement = "right", 
                     trigger = "hover"), 
           div(
             actionButton("show_example_files", "Download example files"),
             icon("info-circle", id = "download_info", class = "info-icon", style = "margin-left: 5px;") # Info icon for download button
           ),
           # Popover for download button info
           bsPopover(id = "download_info",
                     title = "File Information",
                     content = "The files are only suitable for illustrating the file format requirements, but not for identifying real KBAs.",
                     placement = "right",
                     trigger = "hover"),
           uiOutput("download_buttons") 
    ),
    column(6, 
           uiOutput("str_ui"))  # dynamic UI for structure input, will be rendered only if str is uploaded
  ),
  
  hr(),
  
  fluidRow(
    column(12, 
           verbatimTextOutput("output_info")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Distinct genetic diversity"),
      div(
        actionButton("display_genetic_diversity_btn", HTML("Display distinct genetic diversity (Δ<sup>+</sup><i><sub>j</sub></i>)"), style = "margin-bottom: 0px"),
        downloadButton("download_DGD", HTML("Download distinct genetic diversity (Δ<sup>+</sup><i><sub>j</sub></i>) as .csv"))  
      ),
      br(), br(),
      h3("Identify Key Biodiversity Areas (KBAs)"),
      actionButton("threatened_btn", "A1: Threatened species"),
      uiOutput("threatened_options"),
      br(), 
      actionButton("restricted_btn", "B1: Individual geographically restricted species"),
      br(), br(), br(), br(),
      h4("help"), 
      downloadButton("download_manual", "Download manual")
    ),
    
    mainPanel(
      uiOutput("main_content")  # Use uiOutput to conditionally display content
    )
  )
)

shinyApp(ui = ui, server = server)
