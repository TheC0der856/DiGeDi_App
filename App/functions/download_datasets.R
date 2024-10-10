download_datasets <- function() {
  tagList(
    div(style = "margin-top: 20px;"), 
    
    UI_datasets("downloadData", "Download .csv", "csv_info", 
                     "File Information", 
                     "This table is based on the dataset nancycats included in the R package adegenet."),
    
    UI_datasets("downloadGENETIX", "Download .gtx", "gtx_info", 
                     "Data Reference", 
                     "Salmona et al. (2017): Data from: Climate change and human colonization triggered habitat loss and fragmentation in Madagascar."),
    
    UI_datasets("downloadGenpop", "Download .gen", "gen_info", 
                     "Data Reference", 
                     "Millette et al. (2019): Breaking ecological barriers: anthropogenic disturbance leads to habitat transitions."),
    
    UI_datasets("downloadFstat", "Download .dat", "dat_info", 
                     "Data Reference", 
                     "DeFaveri et al. (2013): High degree of genetic differentiation in marine three-spined sticklebacks."),
    
    UI_datasets("downloadSTRUCTURE", "Download .str", "str_info_reference", 
                     "Data Reference", 
                     "Wagner et al. (2012): Recent speciation between sympatric Tanganyikan cichlid color morphs.")
  )
}