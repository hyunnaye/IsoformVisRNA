ui <- fluidPage(
  titlePanel("Shiny App with Barplot and Boxplot"),
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      width = '100%',
      align = "center",

      tags$p("Display bar and box plots from inputed ratio data generated from calculateRatios()."),

      br(),

      tags$p("Upload CSV files that you created with calculateRatios(). It should be in the output_path you declared when running the function."),

      fileInput("ratioDF",
                "Choose CSV File for expression ratio analysis data",
                accept = ".csv"),

      br(),
      radioButtons("plotType", "Select Plot Type:",
                   choices = c("Barplot", "Boxplot"),
                   selected = "Barplot"),
      conditionalPanel(
        condition = "input.plotType == 'Barplot'",
        textInput("xLabel", "X-axis Label:", value = ""),
        textInput("yLabel", "Y-axis Label:", value = ""),
        textInput("plotTitle", "Plot Title:", value = ""),
      ),
      conditionalPanel(
        condition = "input.plotType == 'Boxplot'",
        textInput("xLabel", "X-axis Label:", value = ""),
        textInput("yLabel", "Y-axis Label:", value = ""),
        textInput("plotTitle", "Plot Title:", value = ""),
        textInput("boxOrder", "If you would like to set a custom order for the boxes, write the group names in the order you want, separated by / (no spaces in between). Note all groups must be present. Leave blank for default.", value = ""),
      ),
      actionButton("generateButton", "Generate")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {

  data <- reactive({
    as.data.frame(read.csv(input$ratioDF$datapath,sep = ",",header = TRUE))[,-1]
  })
  boxOrder <- reactive({
    if (input$boxOrder != "") {
      strsplit(input$boxOrder, "/")[[1]]
    } else {
      NULL
    }
  })
  # Render the selected plot
  observeEvent(input$generateButton, {
    output$plot <- renderPlot({
      if (input$plotType == "Barplot") {
        # Example usage of generateBarplot
        generateBarPlot(data(), input$xLabel, input$yLabel, input$plotTitle)
      } else {
        generateBoxPlot(data(), input$xLabel, input$yLabel, input$plotTitle, boxOrder())
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
