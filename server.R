server <- function(input, output) {
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  # Data ----
  callModule(showData, "data")

  # Plot ----
  ## Scatterplot ----
  callModule(plotScatterplot, "scatterplot")

  ## Correlation Plot ----
  callModule(plotCorrelation, "correlation")

  # Statistic ----
  ## Descriptive ----
  callModule(statDescriptive, "descriptive")

  ## Multivariate
  callModule(statMultivariate, "multivariate")
}
