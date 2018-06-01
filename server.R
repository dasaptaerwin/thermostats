server <- function(input, output) {
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
  
  observe({
    toggleState(id = "apply_scatterplot",
                condition = !is.null(input$province) & !is.null(input$x_axis) & !is.null(input$y_axis))
  })
  
  scatterplot <- eventReactive(input$apply_scatterplot, {
    res_plot <- dataset %>% 
      filter(Province == input$province)  %>% 
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
}