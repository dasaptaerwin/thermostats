library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(plotly)
library(ggcorrplot)
library(ggrepel)
library(FactoMineR)

invisible(map(list.files("./module", full.names = TRUE), source))
invisible(map(list.files("./helper/", full.names = TRUE), source))


dataset <- read_csv("./data/data_copy.csv") %>%
  as_tibble() %>%
  select(-Code) %>%
  rename(
    Province = "Prov",
    Location = "Loc"
  )

description <- read_csv("./data/datadescriptor.csv") %>%
  as_tibble() %>%
  select(-`Column no`) %>%
  `[`(-c(1:4, 37:65), )
