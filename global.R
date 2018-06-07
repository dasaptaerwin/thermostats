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

dataset <- read_csv("./data/data_copy.csv") %>%
  as_tibble() %>%
  select(-Code) %>%
  rename(
    Province = "Prov",
    Location = "Loc"
  )

description <- read_csv("./data/datadescriptor.csv") %>%
  as_tibble() %>%
  `[`(-c(1:4, 37:65), -1)
