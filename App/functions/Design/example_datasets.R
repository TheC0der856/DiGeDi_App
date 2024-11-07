UI_datasets <- function(id_button, label_button, id_info, title, content, extra_info_id = NULL, extra_title = NULL, extra_content = NULL) {
  tagList(
    div(style = "display: flex; align-items: center; margin-bottom: 0px;",
        downloadButton(id_button, label_button, style = "width: 150px"),
        icon("info-circle", id = id_info, class = "info-icon", style = "margin-left: 5px;"),
        bsPopover(id = id_info, title = title, content = content, placement = "right", trigger = "hover"),
        
        # in case there is additional information
        if (!is.null(extra_info_id)) {
          tagList(
            icon("info-circle", id = extra_info_id, class = "info-icon", style = "margin-left: 5px;"),
            bsPopover(id = extra_info_id, title = extra_title, content = extra_content, placement = "right", trigger = "hover")
          )
        }
    )
  )
}