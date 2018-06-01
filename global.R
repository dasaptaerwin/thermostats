library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(plotly)

dataset <- read_csv("./data/data_copy.csv") %>% 
  as_tibble() %>% 
  select(-Code) %>% 
  rename(Province = "Prov",
         Location = "Loc")

description <- read_csv("./data/datadescriptor.csv") %>% 
  as_tibble() %>% 
  select(-`Column no`) %>% 
  `[`(-c(1:4, 37:65),)
