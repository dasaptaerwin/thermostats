statDescriptiveUI <- function(id) {
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
        h3("Summary Table"),
        withSpinner(htmlOutput(ns("summary")), type = 4, color = "#44ade9")
      )
    )
  )
}

statDescriptive <- function(input, output, session) {
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
      condition = !is.null(input$parameter)
    )
  })

  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(Province, Location, one_of(input$parameter))
    } else if (input$profile == "Province") {
      dataset %>%
        select(Province, one_of(input$parameter)) %>%
        filter(Province %in% input$province) %>%
        group_by(Province) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Province)
    } else if (input$profile == "Location") {
      dataset %>%
        select(Province, Location, one_of(input$parameter)) %>%
        filter(Location %in% input$location) %>%
        group_by(Location) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Location)
    } else if (input$profile == "Location within Province") {
      dataset %>%
        select(Province, Location, one_of(input$parameter)) %>%
        filter(Province %in% input$province_ops) %>%
        filter(Location %in% input$location_ops) %>%
        group_by(Location) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Location)
    }
  })

  output$summary <- renderUI({
    print(dfSummary(df(), varnumbers = FALSE, graph.magnif = 0.8, style = "multiline"),
      method = "render",
      omit.headings = TRUE,
      bootstrap.css = FALSE,
      custom.css = "./www/custom-summarytools.css",
      footnote = NA
    )
  })
}
