ui <- tagList(
  useShinyjs(),
  inlineCSS(
    "
    #loading-content {
    position: absolute;
    background: #FFFFFF;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    }
    "
  ),
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  hidden(
    div(
      id = "app-content",
      navbarPage(
        "Thermostats",
        theme = shinytheme("cerulean"),
        tabPanel(
          "Data",
          showDataUI("data")
        ),
        navbarMenu(
          "Plot",
          tabPanel(
            "Scatterplot",
            plotScatterplotUI("scatterplot")
          ),
          tabPanel(
            "Correlation Plot",
            plotCorrelationUI("correlation")
          )
        ),
        navbarMenu(
          "Statistic",
          tabPanel(
            "Descriptive",
            statDescriptiveUI("descriptive")
          ),
          tabPanel(
            "Correlation"
          ),
          tabPanel(
            "Regression"
          ),
          tabPanel(
            "Multivariate",
            statMultivariateUI("multivariate")
          )
        ),
        tabPanel(
          "About",
          icon = icon("support"),
          includeMarkdown("README.md")
        )
      )
    )
  )
)
