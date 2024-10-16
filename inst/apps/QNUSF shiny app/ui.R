library(shiny)
library(shinyjs)
library(DT)
jscode <- "shinyjs.refresh_page = function() { history.go(0); }"
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refresh_page"),
  titlePanel(h1(strong("Quick Non-Uniform Space-Filling (QNUSF) Designs"),style = "font-family:'calibri';font-size:20pt",align = "center")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(10,h2(strong("Prepare Data:"), style = "font-family:'arial';font-size:14pt",align = "left")),
        column(2,actionButton("refresh",strong("Refresh",style = "font-family:'arial';font-size:12pt",align = "center")))
      ),
      strong(tags$hr(style="border-top: 2px solid #000000;")),
      h3(strong("Download Demonstration Materials:"), style = "font-family:'arial';font-size:11pt"),
      downloadButton("User_Guide",label = "User Guide",style = "width:260px;"),
      selectInput("download", "Select data to download", choices = c("RSM 2D 1", "RSM 2D 2", "RSM 2D Irregular Area","RSM 3D",
                                                                     "Mixture 3D", "Mixture 4D","Carbon Capture 4D")),
      downloadButton("downloadData"),
      br(),
      h3(strong("Upload Data:"), style = "font-family:'arial';font-size:11pt"),
      fileInput("Upload_file",NULL,buttonLabel = "Upload data",accept = c(".csv")),
      fluidRow(
        column(10,
               numericInput("Dim_input",p("Please enter the dimension of input space.",style = "font-family:'arial';font-size:11pt"),value = NULL,min = 1,step = 1,width = "100%")
        )
      ),
      textOutput("warningC"),
      fluidRow(
        column(5,checkboxInput("mixtureplot",p("Mixture data",style = "font-family:'arial';font-size:11pt"),FALSE)),
        column(5,actionButton("GO",strong("Go",style = "font-family:'arial';font-size:11pt")))
      ),

      h2(strong("QNUSF:"), style = "font-family:'arial';font-size:14pt"),
      strong(tags$hr(style="border-top: 2px solid #000000;")),
            selectInput("Method",strong("Select Method(s):", style = "font-family:'arial';font-size:12pt"),
                  choices = list("\n","QNUSF Minimax"="Minimax",
                                 "QNUSF Maximin"="Maximin"),multiple = T,selected = "\n"),
      shinyFeedback::useShinyFeedback(),
      tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),
      numericInput("Num_runs",label = "Number of runs:",value = 10,min = 1),
      textOutput("warningA"),
      numericInput("Wt_scale",label = "Maximum weight ratio:",value = 5,min = 1),
      textOutput("warningB"),
      actionButton("work",strong("Generate",style = "font-family:'arial';font-size:11pt"))
    ),
    mainPanel(
      tabsetPanel(
        id = "TABS",
        tabPanel(strong("Instructions",style = "font-family:'arial';font-size:14pt"),
                 h2(strong("Step:",
                           style = "font-family:'arial';font-size:16pt")),
                 p("1. Complete the Prepare Data section to generate the Data Visualization tab.",
                   style = "font-family:'arial';font-size:11pt"),
                 p("2. Complete the QNUSF section to generate the minimax and/or maximin designs",
                   style = "font-family:'arial';font-size:11pt"),
                 h2(strong("Notes:",
                           style = "font-family:'arial';font-size:16pt")),
                 img(src='Data example A.png',align = "right",height="20%", width="20%"),
                 p("1. The uploaded data file must be a .CSV file. Meanwhile, the format must match the right example.
                   The last column must be for the weights. After you upload a data file, please enter the number of columns
                   for the input variables. Generally, the number columns in the file should be (dimension of input space) + 1 (for weights).",
                   style = "font-family:'arial';font-size:11pt"),
                 p("2. To start with new data, please click the 'Refresh' button to reset the environment.",
                   style = "font-family:'arial';font-size:11pt"),
                 p("3. For 1-D, 2-D & 3-D RSM, and 3-D & 4-D mixture examples, the visualization given is a contour plot.
                   For 3-D RSM and 4-D mixture examples, the visualization is a sliced plot. For cases other than those listed,
                   only the created design is provided (with no visualization).",
                   style = "font-family:'arial';font-size:11pt"),
                 em("For more details, please see User Guide (on left).",
                    style = "font-family:'arial';font-size:11pt")
        ),
        tabPanel( strong("Data Visualization",style = "font-family:'arial';font-size:14pt"),
                  numericInput("n", strong("Rows",style = "font-family:'arial';font-size:12pt"), value = 5, min = 1, step = 1),
                  DT::dataTableOutput("head"),
                  conditionalPanel(
                    condition = "input.mixtureplot == 0",
                    plotOutput("cand_plot")
                  ),
                  conditionalPanel(
                    condition = "input.mixtureplot == 1",
                    plotOutput("cand_mixed_plot")
                  )
        ),
        tabPanel(strong("Minimax",style = "font-family:'arial';font-size:14pt"),value = "Minimax",
                 span(htmlOutput("mM_info"),style = "font-family:'arial';font-size:14pt",align = "center"),
                 br(),
                 fluidRow(
                   column(3,align = "left",p(strong("Design Table", style = "font-family:'arial';font-size:12pt"))),
                   column(5,align = "center",numericInput("Round1", strong("Decimal",style = "font-family:'arial';font-size:12pt"), value = 3, min = 1, step = 1)),
                   column(3,align = "right",downloadButton("downloadMinimaxDesign",strong("download",style = "font-family:'arial';font-size:11pt")))
                 ),
                 DT::dataTableOutput("Minimax_design"),
                 # tableOutput("Minimax_design"),
                 conditionalPanel(
                   condition = "input.mixtureplot == 0",
                   plotOutput("mM_plot")
                 ),
                 conditionalPanel(
                   condition = "input.mixtureplot == 1",
                   plotOutput("mM_mixed_plot")
                 )),
        tabPanel(strong("Maximin",style = "font-family:'arial';font-size:14pt"),value = "Maximin",
                 span(htmlOutput("Mm_info"),style = "font-family:'arial';font-size:14pt",align = "center"),
                 br(),
                 fluidRow(
                   column(3,align = "left",p(strong("Design Table", style = "font-family:'arial';font-size:12pt"))),
                   column(5,align = "center",numericInput("Round2", strong("Decimal",style = "font-family:'arial';font-size:12pt"), value = 3, min = 1, step = 1)),
                   column(3,align = "right",downloadButton("downloadMaximinDesign",strong("download",style = "font-family:'arial';font-size:11pt")))

                 ),
                 DT::dataTableOutput("Maximin_design"),
                 # tableOutput("Maximin_design"),
                 conditionalPanel(
                   condition = "input.mixtureplot == 0",
                   plotOutput("Mm_plot")
                 ),
                 conditionalPanel(
                   condition = "input.mixtureplot == 1",
                   plotOutput("Mm_mixed_plot")
                 ))
      )
      )
  )
)
