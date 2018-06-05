plotCorrelationUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        tagList(icon = icon("sliders", "fa-2x")),
        br(),
        pickerInput(
          ns("province"),
          "Please select Province to plot",
          choices = unique(dataset$Province),
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        pickerInput(
          ns("parameter"),
          "Please select several parameters to investigate",
          choices = colnames(dataset)[-c(1, 2)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        materialSwitch(
          ns("corrvalue"),
          "Show correlation values",
          status = "primary",
          right = TRUE
        ),
        materialSwitch(
          ns("significant"),
          "Mark insignificant values",
          status = "primary",
          right = TRUE
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            icon = icon("image"),
            h3("Correlation Plot"),
            withSpinner(plotOutput(ns("plot")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Data on Correlation Plot"),
            withSpinner(DT::dataTableOutput(ns("data")), type = 4, color = "#44ade9")
          )
        )
      )
    )
  )
}

plotCorrelation <- function(input, output, session) {
  ns <- session$ns

  # Plot ----
  observe({
    toggleState(
      id = "apply",
      condition = !is.null(input$province) & !is.null(input$parameter)
    )
  })

  corrplot <- eventReactive(input$apply, {
    if (input$significant) {
      p_mat <- dataset %>%
        filter(Province == input$province) %>%
        select(input$parameter) %>%
        ggcorrplot::cor_pmat()
      res_plot <- dataset %>%
        filter(Province == input$province) %>%
        select(input$parameter) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(
          method = "square",
          ggtheme = theme_minimal,
          outline.color = "white",
          lab = input$corrvalue,
          p.mat = p_mat
        )
    } else {
      res_plot <- dataset %>%
        filter(Province == input$province) %>%
        select(input$parameter) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(
          method = "square",
          ggtheme = theme_minimal,
          outline.color = "white",
          lab = input$corrvalue,
          p.mat = NULL
        )
    }
    return(res_plot)
  })

  output$plot <- renderPlot({
    corrplot()
  })

  # Data ----
  plot_data <- eventReactive(input$apply, {
    dataset %>%
      filter(Province == input$province) %>%
      select(input$parameter)
  })

  output$data <- DT::renderDataTable({
    plot_data() %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Download"
              )
            )
          ,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#44ade9', 'color': '#fff'});",
            "}"
          )
        )
      )
  },
  server = FALSE
  )
}
