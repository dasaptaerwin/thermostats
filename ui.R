library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(plotly)
library(ggcorrplot)
library(summarytools)
library(intubate)
library(ggrepel)
library(FactoMineR)

invisible(map(list.files("./modules", full.names = TRUE), source))
invisible(map(list.files("./helpers", full.names = TRUE), source))

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
            plotScatterplotUI("plot_scatterplot")
          ),
          tabPanel(
            "Correlation Plot",
            plotCorrelationUI("plot_correlation")
          )
        ),
        navbarMenu(
          "Statistic",
          tabPanel(
            "Descriptive",
            statDescriptiveUI("stat_descriptive")
          ),
          tabPanel(
            "Correlation",
            statCorrelationUI("stat_correlation")
          ),
          tabPanel(
            "Regression",
            statRegressionUI("stat_regression")
          ),
          tabPanel(
            "Multivariate",
            statMultivariateUI("stat_multivariate")
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
