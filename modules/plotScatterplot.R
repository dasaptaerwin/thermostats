plotScatterplotUI <- function(id) {
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
          ns("x_axis"),
          "Please select one parameter as x axis:",
          choices = colnames(dataset)[-c(1, 2)],
          selected = "Ca"
        ),

        pickerInput(
          ns("y_axis"),
          "Please select one parameter as y axis:",
          choices = colnames(dataset)[-c(1, 2)],
          selected = "pH"
        ),
        materialSwitch(
          ns("pointscolour"),
          "Colour points by Province",
          status = "primary",
          right = TRUE
        ),
        materialSwitch(
          ns("regressionline"),
          "Show regression line",
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
            h3("Scatterplot"),
            withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Data on Scatterplot"),
            withSpinner(DT::dataTableOutput(ns("data")), type = 4, color = "#44ade9")
          )
        )
      )
    )
  )
}

plotScatterplot <- function(input, output, session) {
  ns <- session$ns

  # Plot ----
  observe({
    toggleState(
      id = "apply",
      condition = !is.null(input$province) & !is.null(input$x_axis) & !is.null(input$y_axis)
    )
  })

  scatterplot <- eventReactive(input$apply, {
    res_plot <- dataset %>%
      filter(Province == input$province) %>%
      ggplot(aes_string(x = input$x_axis, y = input$y_axis)) +
      geom_point(aes_string(col = if_else(input$pointscolour, "Province", "NULL"))) +
      labs(col = "") +
      theme_minimal()

    if (input$regressionline) {
      res_plot <- res_plot + geom_smooth(method = "lm", na.rm = TRUE)
    } else {
      res_plot
    }
    return(res_plot)
  })

  output$plot <- renderPlotly({
    scatterplot() %>%
      ggplotly()
  })

  # Data ----
  plot_data <- eventReactive(input$apply, {
    dataset %>%
      filter(Province == input$province) %>%
      select(input$x_axis, input$y_axis)
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
