statDescriptiveUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        tagList(icon = icon("sliders", "fa-2x")),
        br(),
        pickerInput(
          ns("province"),
          "Please select Province to analyse",
          choices = unique(dataset$Province),
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        pickerInput(
          ns("location"),
          "Please select Location within Province to analyse",
          choices = NULL,
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        pickerInput(
          ns("parameter"),
          "Please select parameter to investigate",
          choices = colnames(dataset)[-c(1, 2)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            icon = icon("image"),
            h3("Correlation Plot"),
            tableOutput(ns("summary")),
            withSpinner(plotOutput(ns("plot")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Data on Correlation Plot"),
            withSpinner(DT::dataTableOutput(ns("dataset")), type = 4, color = "#44ade9")
          )
        )
      )
    )
  )
}

statDescriptive <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$province, {
    loc <- dataset %>% 
      filter(Province %in% input$province) %>% 
      select(Location) %>% 
      unique() %>% 
      pull()
    updatePickerInput(
      session = session, 
      inputId = "location", 
      choices = loc
    )
  },
  ignoreInit = TRUE
  )
  
  summary <- eventReactive(input$apply, {
    dataset %>% 
    {
      tibble(
        "Province" = input$province,
        "Location" = input$location,
        "Parameter" = input$parameter
      )
    }
  })
  
  output$summary <- renderTable({
    summary()
  }, colnames = FALSE)
} 