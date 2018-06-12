statCorrelationUI <- function(id) {
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
          ns("x"),
          "Please select a parameter to investigate",
          choices = colnames(dataset)[-c(1, 2)],
          selected = "Mg",
          options = list(
            "live-search" = TRUE
          )
        ),
        pickerInput(
          ns("y"),
          "Please select another parameter to investigate",
          choices = colnames(dataset)[-c(1, 2)],
          selected = "Na",
          options = list(
            "live-search" = TRUE
          )
        ),
        radioButtons(
          ns("alternative"),
          "Alternative Hypothesis",
          choices = c(
            "Two-sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          )
        ),
        radioButtons(
          ns("method"),
          "Test Method",
          choices = c(
            "Pearson" = "pearson",
            "Kendall" = "kendall",
            "Spearman" = "spearman"
          )
        ),
        numericInput(
          ns("conf.level"),
          "Confidence Level",
          min = 0.8, max = 1, value = 0.95
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        h3("Summary of Statistic"),
        withSpinner(tableOutput(ns("result")), type = 4, color = "#44ade9"),
        hr(),
        h3("Plot"),
        withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
      )
    )
  )
}

statCorrelation <- function(input, output, session) {
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
      condition = !is.null(input$x) & !is.null(input$y)
    )
  })

  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(Province, Location, one_of(input$x, input$y))
    } else if (input$profile == "Province") {
      dataset %>%
        select(Province, one_of(input$x, input$y)) %>%
        filter(Province %in% input$province)
    } else if (input$profile == "Location") {
      dataset %>%
        select(Province, Location, one_of(input$x, input$y)) %>%
        filter(Location %in% input$location)
      rename(Profile = Location)
    } else if (input$profile == "Location within Province") {
      dataset %>%
        select(Province, Location, one_of(input$x, input$y)) %>%
        filter(Province %in% input$province_ops) %>%
        filter(Location %in% input$location_ops)
    }
  })

  result <- eventReactive(input$apply, {
    formula <- paste("~", input$x, "+", input$y)
    df() %>%
      ntbt(cor.test, formula = as.formula(formula), alternative = input$alternative, method = input$method, conf.level = input$conf.level) %>%
      tidy() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })

  output$result <- renderTable({
    result()
  })

  scatterplot <- eventReactive(input$apply, {
    df() %>%
      ggplot(aes_string(x = input$x, y = input$y)) +
      geom_point() +
      geom_smooth(method = "lm", na.rm = TRUE) +
      labs(col = "") +
      theme_minimal()
  })

  output$plot <- renderPlotly({
    scatterplot() %>%
      ggplotly()
  })
}
