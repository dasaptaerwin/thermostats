ui <- tagList(
  useShinyjs(),
  navbarPage(
    "Thermostat",
    theme = shinytheme("cerulean"),
    tabPanel(
      "Data",
      tabsetPanel(
        tabPanel(
          "Dataset",
          icon = icon("table"),
          br(),
          withSpinner(DT::dataTableOutput("dataset"), type = 4, color = "#44ade9")
        ),
        tabPanel(
          "Parameter Description",
          icon = icon("bookmark"),
          br(),
          withSpinner(DT::dataTableOutput("description"), type = 4, color = "#44ade9")
        )
      )
    ),
    navbarMenu(
      "Plot",
      tabPanel(
        "Scatterplot",
        sidebarLayout(
          sidebarPanel(
            tagList(icon = icon("sliders", "fa-2x")),
            br(),
            pickerInput(
              "province_scatterplot",
              "Please select Province to plot",
              choices = unique(dataset$Province),
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE
              )
            ),
            pickerInput(
              "x_axis",
              "Please select one parameter as x axis:",
              choices = colnames(dataset)[-c(1, 2)],
              selected = "Ca"
            ),

            pickerInput(
              "y_axis",
              "Please select one parameter as y axis:",
              choices = colnames(dataset)[-c(1, 2)],
              selected = "pH"
            ),
            materialSwitch(
              "pointscolour",
              "Colour points by Province",
              status = "primary",
              right = TRUE
            ),
            materialSwitch(
              "regressionline",
              "Show regression line",
              status = "primary",
              right = TRUE
            ),
            actionButton("apply_scatterplot", "Apply")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                "Plot",
                icon = icon("image"),
                h3("Scatterplot"),
                withSpinner(plotlyOutput("scatterplot"), type = 4, color = "#44ade9")
              ),
              tabPanel(
                "Data",
                icon = icon("table"),
                h3("Data on Scatterplot"),
                withSpinner(DT::dataTableOutput("scatterplot_data"), type = 4, color = "#44ade9")
              )
            )
          )
        )
      ),
      tabPanel(
        "Correlation Plot",
        sidebarLayout(
          sidebarPanel(
            tagList(icon = icon("sliders", "fa-2x")),
            br(),
            pickerInput(
              "province_corrplot",
              "Please select Province to plot",
              choices = unique(dataset$Province),
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE
              )
            ),
            pickerInput(
              "parameters",
              "Please select several parameters to investigate",
              choices = colnames(dataset)[-c(1, 2)],
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE
              )
            ),
            materialSwitch(
              "corrvalue",
              "Show correlation values",
              status = "primary",
              right = TRUE
            ),
            materialSwitch(
              "significant",
              "Mark insignificant values",
              status = "primary",
              right = TRUE
            ),
            actionButton("apply_corrplot", "Apply")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                "Plot",
                icon = icon("image"),
                h3("Correlation Plot"),
                withSpinner(plotOutput("corrplot"), type = 4, color = "#44ade9")
              ),
              tabPanel(
                "Data",
                icon = icon("table"),
                h3("Data on Correlation Plot"),
                withSpinner(DT::dataTableOutput("corrplot_data"), type = 4, color = "#44ade9")
              )
            )
          )
        )
      )
    ),
    # tabPanel(
    #   "Scatterplot",
    #   sidebarLayout(
    #     sidebarPanel(
    #       tagList(icon = icon("sliders", "fa-2x")),
    #       br(),
    #       pickerInput(
    #         "province",
    #         "Please select Province to plot",
    #         choices = unique(dataset$Province),
    #         multiple = TRUE,
    #         options = list(
    #           'actions-box' = TRUE
    #         )
    #       ),
    #       pickerInput(
    #         "x_axis",
    #         "Please select one parameter as x axis:",
    #         choices = colnames(dataset)[-c(1,2)],
    #         selected = "Ca"
    #       ),
    #
    #       pickerInput(
    #         "y_axis",
    #         "Please select one parameter as y axis:",
    #         choices = colnames(dataset)[-c(1,2)],
    #         selected = "pH"
    #       ),
    #       materialSwitch(
    #         "pointscolour",
    #         "Colour points by Province?",
    #         status = "primary",
    #         right = TRUE),
    #       materialSwitch(
    #         "regressionline",
    #         "Show regression line",
    #         status = "primary",
    #         right = TRUE),
    #       actionButton("apply_scatterplot", "Apply")
    #     ),
    #     mainPanel(
    #       h3("Scatterplot"),
    #       withSpinner(plotlyOutput("scatterplot"), type = 4, color = "#44ade9")
    #     )
    #   )
    # ),
    navbarMenu(
      "Statistic",
      tabPanel("Descriptive"),
      tabPanel("Correlation"),
      tabPanel("Regression")
    ),
    tabPanel(
      "About",
      icon = icon("support"),
      includeMarkdown("README.md")
    )
  )
)
