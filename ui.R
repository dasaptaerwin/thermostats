library(shinydashboard)
library(shinyWidgets)
library(DT)

header <- dashboardHeader(title = strong("Thermostats"))
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Beranda", 
             icon = icon("home"), 
             tabName = "beranda"),
    menuItem("Dataset",
           icon = icon("database"),
           tabName = "dataset"),
    menuItem("Grafik",
           icon = icon("image"),
           tabName = "grafik")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "beranda",
            fluidRow(
              box(
                title = tagList(icon = icon("info-circle"), "Pengantar"),
                  width = 12,
                status = "primary",
                  solidHeader = TRUE,
                p("Isi pengantar disini")),
              box(
                title = tagList(icon = icon("edit"), "Sitasi"),
                width = "6",
                status = "primary",
                solidHeader = TRUE,
                p("Cara sitasi")
              ),
              box(
                title = tagList(icon = icon("users"), "Kontributor"),
                width = "6",
                status = "primary",
                solidHeader = TRUE,
                p("Nama kontributor")
              )
            )),
    tabItem(tabName = "dataset",
            fluidRow(
              tabBox(
                title = tagList(icon("table"), "Dataset"),
                width = 12,
                tabPanel("Data",
                         dataTableOutput("data")),
                tabPanel("Deskripsi Data",
                         dataTableOutput("deskripsi"))
                  )
            )),
    tabItem(tabName = "grafik",
            fluidRow(
              box(
                title = "",
                width = 12,
                plotOutput("grafik"),
                uiOutput("simpan")
              ),
              box(
                title = tagList(icon = icon("sliders"), "Pengaturan"),
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                selectInput(inputId = "jenis_grafik", 
                            label = "Jenis Grafik", 
                            choices = c(
                              "Grafik Batang" = "batang",
                              "Grafik Garis" = "garis",
                              "Grafik Titik" = "titik"
                            )),
                selectInput(inputId = "sumbu_x", 
                            label = "Sumbu X", 
                            choices = colnames(dat)),
                selectInput(inputId = "sumbu_y", 
                            label = "Sumbu Y", 
                            choices = colnames(dat)),
                actionButton(inputId = "terapkan", 
                             label = "Terapkan")),
              box(
                title = tagList(icon = icon("cogs"), "Estetika"),
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                textInput(
                  inputId = "grafik_judul",
                  label = "Judul Grafik"),
                textInput(
                  inputId = "grafik_xlab",
                  label = "Label sumbu x"),
                textInput(
                  inputId = "grafik_ylab",
                  label = "Label sumbu y")
                
                
              )
            ))
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)
