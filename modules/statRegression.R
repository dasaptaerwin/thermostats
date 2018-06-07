statRegressionUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        tagList(icon = icon("sliders", "fa-2x")),
        br(),
        radioButtons(
          ns("profile"),
          "Please select profile of interest",
          choices = c(
            "All",
            "Province",
            "Location",
            "Location within Province"
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Province'",
          ns = ns,
          pickerInput(
            ns("province"),
            "Please select Province to analyse",
            choices = unique(dataset$Province),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Location'",
          ns = ns,
          pickerInput(
            ns("location"),
            "Please select Location to analyse",
            choices = unique(dataset$Location),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Location within Province'",
          ns = ns,
          pickerInput(
            ns("province_ops"),
            "Please select Province to analyse",
            choices = unique(dataset$Province)
          ),
          pickerInput(
            ns("location_ops"),
            "Please select Location within Province to analyse",
            choices = unique(dataset[dataset$Province == "Jabar", "Location"]),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        pickerInput(
          ns("dependent"),
          "Please select a dependent variable",
          choices = colnames(dataset)[-c(1, 2)],
          selected = "Mg",
          options = list(
            "live-search" = TRUE
          )
        ),
        pickerInput(
          ns("independent"),
          "Please select one or several independent variable(s)",
          choices = colnames(dataset)[-c(1, 2)],
          selected = c("Na", "Br"),
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE,
            "live-search" = TRUE
          )
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        h3("Summary of Statistic"),
        withSpinner(tableOutput(ns("result")), type = 4, color = "#44ade9"),
        withSpinner(tableOutput(ns("result2")), type = 4, color = "#44ade9"),
        hr(),
        h3("Plot"),
        withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
      )
    )
  )
}

statRegression <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$province_ops, {
    loc <- dataset %>%
      filter(Province %in% input$province_ops) %>%
      select(Location) %>%
      unique() %>%
      pull()
    updatePickerInput(
      session = session,
      inputId = "location_ops",
      choices = loc
    )
  },
  ignoreInit = TRUE
  )
  
  observe({
    toggleState(
      id = "apply",
      condition = !is.null(input$dependent) & !is.null(input$independent)
    )
  })
  
  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(Province, Location, one_of(input$dependent, input$independent))
    } else if (input$profile == "Province") {
      dataset %>%
        select(Province, one_of(input$dependent, input$independent)) %>%
        filter(Province %in% input$province)
    } else if (input$profile == "Location") {
      dataset %>%
        select(Province, Location, one_of(input$dependent, input$independent)) %>%
        filter(Location %in% input$location)
      rename(Profile = Location)
    } else if (input$profile == "Location within Province") {
      dataset %>%
        select(Province, Location, one_of(input$dependent, input$independent)) %>%
        filter(Province %in% input$province_ops) %>%
        filter(Location %in% input$location_ops)
    }
  })
  
  result <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      tidy() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })
  
  output$result <- renderTable({
    result()
  })
  
  result2 <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      glance() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })
  
  output$result2 <- renderTable({
    result2()
  })
  
  estimateplot <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      tidy(conf.int = TRUE) %>% 
      `colnames<-`(tools::toTitleCase(names(.))) %>% 
      ggplot(aes(Estimate, Term, color = Term)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = Conf.low, xmax = Conf.high), height = 0.3) +
      labs(col = "") +
      theme_minimal()
  })

  output$plot <- renderPlotly({
    estimateplot() %>%
      ggplotly()
  })
}
