require(shiny)
require(googleVis)
require(gridExtra)
# require(Kmisc)
# require(rCharts)
# require(rNVD3)
loadingBar <- tags$div(class="progress progress-striped active",
                       tags$div(class="bar", style="width: 100%;"))
# Code for loading message
loadingMsg <- tags$div(class="modal", tabindex="-1", role="dialog", 
                       "aria-labelledby"="myModalLabel", "aria-hidden"="true",
                       tags$div(class="modal-header",
                                tags$h3(id="myModalHeader", "Loading...")),
                       tags$div(class="modal-footer",
                                loadingBar))
# The conditional panel to show when shiny is busy
loadingPanel <- conditionalPanel(paste("input.goButton > 0 &&", 
                                       "$('html').hasClass('shiny-busy')"),
                                 loadingMsg)

shinyUI(fluidPage(
#   headerPanel(
#     list(tags$head(tags$style("body {background-color: black; }")),
#          "Graphs", HTML('<img src="ah_large_black.gif", height="200px"    
#                         style="float:right"/>','<p style="color:red"> test test </p>' ))
#     ),
  titlePanel("Hill's Curve Estimation"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(   
        tags$style(type="text/css", "textarea { max-width: 200px; }"),
        tags$style(type='text/css', ".span4 { max-width: 300px; }"),
        tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")
      ),
      
      p(h4("Data setting")),
      wellPanel(
        selectInput(inputId='datatype', label='Choose the data type', 
                    choices=c("Abundance data"="abun", "Incidence data"="inci")
        ),
        radioButtons(inputId="source", "Choose one:", 
                     choices=c("Key in data"="keyin", "Upload data"="upload")
        ),
        uiOutput("choose_dataset"),
        p(em("Using ctrl / command key to select multiple datasets you want")),
        
        conditionalPanel(condition="input.source == 'upload'", 
                         fileInput(inputId="files", label="File data")
        ),
        conditionalPanel(condition="input.source == 'keyin'", 
                         conditionalPanel(
                           condition = "input.datatype == 'abun'",
                           tags$textarea(id="copyAndPaste_abun", rows = 8,
                                         "Girdled 46 22 17 15 15  9  8  6  6  4  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1 \nLogged 88 22 16 15 13 10  8  8  7  7  7  5  4  4  4  3  3  3  3  2  2  2  2  1  1  1  1  1  1  1  1  1  1  1  1  1  1"),
                           p(em("(Input format : Species abundance frequency.)"))
                         ),
                         conditionalPanel(
                           condition = "input.datatype == 'inci'",
                           tags$textarea(id="copyAndPaste_inci", rows = 8,
                                         "Ants_1500m 200 144 113 79 76 74 73 53 50 43 33 32 30 29 25 25 25 24 23 23 19 18 17 17 11 11 9 9 9 9 6 6 5 5 5 5 4 4 3 3 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 \nAnts_2000m 200 80 59 34 23 19 15 13 8 8 4 3 2 2 1"),
                           p(em("(Input format : First entry should be the total number of sampling units, and followed by species incidence frequency.)"))
                         )     
        )
#         checkboxGroupInput(inputId='method', label='Choose the estimated method(s)', 
#                            choices=c("all", "MLE", "Chao"), selected="all")
      ),
      
      
      p(h4("General setting")),
      wellPanel(
        sliderInput(inputId='orderq', label='Order q (default is 0 to 2)', min=0, max=5, value=c(0, 2), step=0.1),
        numericInput(inputId='conf', label='Confidence level (default is 0.95)', value=0.95, min=0, max=1, step=0.01), 
        numericInput(inputId='cutpt', label='cut-off point (default is 10)', value=10, min=1, max=20, step=1), 
        numericInput(inputId='nboot', label='Number of Bootstrap (default is 5)', value=5, min=5, max=1000, step=5)
      )
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data summary", h3("Basic data information"),
                 loadingPanel,
                 htmlOutput("datainfo")
                 ),
        tabPanel("Estimtation", h3("Estimation"),
                 checkboxInput("Showq012", "Only show the integral of q.", TRUE),
                 loadingPanel, 
                 htmlOutput('estimation')
                 ),
        tabPanel("Figure plot (by Data)", h3(""),
                 loadingPanel,
                 plotOutput("myPlot1", width="700px", height="400px")
                 ), 
        tabPanel("Figure plot (by Method)", h3(""),
                 loadingPanel,
                 plotOutput("myPlot2", width="700px", height="auto")
                 ), 
        tabPanel("User Guide"
                 #includeMarkdown("man/user_guide.Rmd")
                 )


      )
    )
    # end of panel
    
  )
  
  
))