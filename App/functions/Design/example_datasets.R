UI_datasets <- function(id_button, label_button, id_info, title, content) {
  tagList(
    div(style = "display: flex; align-items: center; margin-bottom: 0px;",  
        downloadButton(id_button, label_button, style = "width: 150px"),
        icon("info-circle", id = id_info, class = "info-icon", style = "margin-left: 5px;")
    ),
    bsPopover(id = id_info, title = title, content = content, placement = "right", trigger = "hover")
  )
}