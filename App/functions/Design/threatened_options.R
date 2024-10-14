threatened_options_UI <- function() {
  tagList(
    div(class = "header-with-image",
        h4("Critically Endangered"),
        img(src = "CR.png", height = "25px"),
        h4("or Endangered"),
        img(src = "EN.png", height = "25px")
    ),
    actionButton("A1a_button", HTML('<div class="btn-with-image">
                                      A1a: <img src="CR.png" height="18px"/>
                                      <span style="font-size: 20px; margin: 0px;">/</span> 
                                      <img src="EN.png" height="18px"/>
                                      </div>')),
    actionButton("A1c_button", HTML('<div class="btn-with-image">
                                      A1c: <img src="CR.png" height="18px"/>
                                      <span style="font-size: 20px; margin: 0px;">/</span> 
                                      <img src="EN.png" height="18px" style="margin-right: 16px;"/>
                                      due only to past/current decline [Red List A only, but not A3 only]
                                    </div>')), 
    div(class = "header-with-image",
        h4("Vulnerable"),
        img(src = "VU.png", height = "25px")
    ),
    actionButton("A1b_button", HTML('<div class="btn-with-image">
                                      A1b:<img src="VU.png" height="18px"/></div>')),
    actionButton("A1d_button", HTML('<div class="btn-with-image">
                                      A1d:<img src="VU.png" height="18px" style="margin-right: 36px;"/>
                                      due only to past/current decline [Red List A only, but not A3 only]
                                    </div>'))
  )
}