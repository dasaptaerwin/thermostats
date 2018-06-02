statMultivariateUI <- function(id) {
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
        pickerInput(
          ns("parameter_supp"),
          "Please select supplementary parameter to investigate",
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
            "Overview",
            icon = icon("briefcase"),
            h3("Overview"),
            withSpinner(tableOutput(ns("overview")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Agggregated dataset"),
            withSpinner(DT::dataTableOutput(ns("dataset")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Dimension",
            icon = icon("plus"),
            h3("Description of dimension"),
            withSpinner(verbatimTextOutput(ns("dimension")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Plot",
            icon = icon("image"),
            h3("PCA Plot"),
            fluidRow(
              column(
                1,
                dropdownButton(
                  circle = TRUE,
                  status = "primary",
                  icon = icon("gear"),
                  width = "350px",
                  tooltip = tooltipOptions(title = "Options"),
                  selectInput(
                    inputId = ns("options"),
                    label = "Plot Options",
                    choices = c("Screeplot", "Profile", "Parameter")
                  ),
                  numericInput(
                    inputId = ns("x_axis"),
                    label = "Dimension on x axis",
                    min = 1,
                    max = 4,
                    value = 1
                  ),
                  numericInput(
                    inputId = ns("y_axis"),
                    label = "Dimension on y axis",
                    min = 2,
                    max = 5,
                    value = 2
                  )
                )
              ),
              column(11,
                     align = "center",
                     withSpinner(plotOutput(ns("plot"), width = "600px", height = "400px"), type = 4, color = "#44ade9")
              )
            )
          )
        )
      )
    )
  )
}

statMultivariate <- function(input, output, session) {
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
    if (input$profile == "Province") {
      dataset %>%
        filter(Province %in% input$province) %>% 
        select(Province, input$parameter, input$parameter_supp) %>%
        group_by(Province) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Province)
    } else if (input$profile == "Location") {
      dataset %>%
        filter(Province %in% input$location) %>% 
        select(Province, Location, input$parameter, input$parameter_supp) %>%
        group_by(Location) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Location)
    } else if (input$profile == "Location within Province") {
      dataset %>%
        select(Province, Location, input$parameter, input$parameter_supp) %>%
        filter(Province %in% input$province_ops) %>% 
        filter(Location %in% input$location_ops) %>%
        group_by(Location) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = Location)
    }
  })

  overview <- eventReactive(input$apply, {
    df() %>%
      {
        tibble(
          "Method" = "Principal Component Analysis",
          "Profile" = .[, "Profile"] %>% pull() %>% unique() %>% paste(collapse = ", "),
          "Parameter" = paste(input$parameter, collapse = ", "),
          "Paramater Supplementary" = paste(input$parameter_supp, collapse = ", ")
        )
      } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })

  output$overview <- renderTable({
    overview()
  }, colnames = FALSE)
  
  output$dataset <- DT::renderDataTable({
    df() %>%
      mutate_if(is.numeric, round, 2) %>% 
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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
  
  global <- reactive({
    req(df())
    if (is.null(input$parameter_supp)) {
      dat <- df() %>%
        as.data.frame() %>%
        `rownames<-`(.[, "Profile"]) %>%
        select_if(is.numeric)
      quanti_supp <- NULL
    } else {
      dat <- df() %>% 
        as.data.frame() %>%
        `rownames<-`(.[, "Profile"]) %>%
        select_if(is.numeric)
      quanti_supp <- which(names(dat) %in% input$parameter_supp)
    }
    res <- PCA(dat, quanti.sup = quanti_supp, graph = FALSE)
    return(res)
  })
  
  output$dimension <- renderPrint({
    dimdesc(global(), proba = 1)
  })
  
  plot <- reactive({
    if (input$options == "Screeplot") {
      plot_eigen(global())
    } else if (input$options == "Profile") {
      plot_profile(global(),
                   axes = c(input$x_axis, input$y_axis))
    } else if (input$options == "Parameter") {
      plot_parameter(res = global(),
                     axes = c(input$x_axis, input$y_axis))
    }
  })
  
  output$plot <- renderPlot({
    plot()
  })
}
