fluidPage(
tags$b(tags$h1("Coursera Data Science Capstone",align="center")),
tags$hr(),
tags$br(),
wellPanel(
tags$div(id="pane",
textInput("word",tags$b("Enter Phrase or Word"),value=""),
actionButton("submit","Submit"),align="center"
)),
tags$div(uiOutput("disp"),align="center")
)