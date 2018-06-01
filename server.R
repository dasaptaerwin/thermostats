server <- function(input, output) {
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  # Data ----
  ## Dataset ----
  output$dataset <- DT::renderDataTable({
    dataset %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brfti",
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

  ## Data Description ----
  output$description <- DT::renderDataTable({
    description %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brfti",
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

  # Plot ----
  ## Scatterplot ----
  ### Plot ----
  observe({
    toggleState(
      id = "apply_scatterplot",
      condition = !is.null(input$province_scatterplot) & !is.null(input$x_axis) & !is.null(input$y_axis)
    )
  })

  scatterplot <- eventReactive(input$apply_scatterplot, {
    res_plot <- dataset %>%
      filter(Province == input$province_scatterplot) %>%
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

  output$scatterplot <- renderPlotly({
    scatterplot() %>%
      ggplotly()
  })

  ### Data ----
  scatterplot_data <- eventReactive(input$apply_scatterplot, {
    dataset %>%
      filter(Province == input$province_scatterplot) %>%
      select(input$x_axis, input$y_axis)
  })

  output$scatterplot_data <- DT::renderDataTable({
    scatterplot_data() %>%
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

  ## Correlation Plot ----
  ### Plot ----
  observe({
    toggleState(
      id = "apply_corrplot",
      condition = !is.null(input$province_corrplot) & !is.null(input$parameters)
    )
  })

  corrplot <- eventReactive(input$apply_corrplot, {
    if (input$significant) {
      p_mat <- dataset %>%
        filter(Province == input$province_corrplot) %>%
        select(input$parameters) %>%
        ggcorrplot::cor_pmat()
      res_plot <- dataset %>%
        filter(Province == input$province_corrplot) %>%
        select(input$parameters) %>%
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
        filter(Province == input$province_corrplot) %>%
        select(input$parameters) %>%
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

  output$corrplot <- renderPlot({
    corrplot()
  })

  ### Data ----
  corrplot_data <- eventReactive(input$apply_corrplot, {
    dataset %>%
      filter(Province == input$province_corrplot) %>%
      select(input$parameters)
  })

  output$corrplot_data <- DT::renderDataTable({
    corrplot_data() %>%
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

  callModule(statDescriptive, "descriptive")
}
