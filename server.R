library(tidyverse)

server <- function(input, output){
  dat <- read_csv("data_copy.csv") %>% 
    as_tibble() %>% 
    rename(Kode = "Code",
           Provinsi = "Prov",
           Lokasi = "Loc")
  deskripsi_dat <- read_csv("datadescriptor.csv") %>% 
    as_tibble() %>% 
    select(-`Column no`)
  
  output$data <- renderDataTable({
    datatable(dat,
              options = list(
                lengthMenu = list(c(5, 10, 20, -1), c("5", "10", "20", "All")),
                pageLength = 10,
                searching = FALSE,
                autoWidth = FALSE,
                scrollX = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                  "}"
                )
              ),
              rownames = FALSE
    )
  })
  
  output$deskripsi <- renderDataTable({
    datatable(deskripsi_dat,
              options = list(
                lengthMenu = list(c(5, 10, 20, -1), c("5", "10", "20", "All")),
                pageLength = 10,
                searching = FALSE,
                autoWidth = FALSE,
                scrollX = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
                  "}"
                )
              ),
              rownames = FALSE
    )
  })
  
  
  grafik <- eventReactive(input$terapkan, {
    if (input$jenis_grafik == "batang") {
      ggplot(dat,
             aes(x = input$sumbu_x,
                 y = input$sumbu_y)) + 
      geom_bar()
    }
    if (input$jenis_grafik == "garis") {
      ggplot(dat,
             aes(x = input$sumbu_x,
                 y = input$sumbu_y)) + 
        geom_line()
    }
    if (input$jenis_grafik == "titik") {
      ggplot(dat,
             aes(x = input$sumbu_x,
                 y = input$sumbu_y)) + 
        geom_point()
    }
  })
  
  output$grafik <- renderPlot({
    plot(grafik())
  })
  
  observeEvent(input$terapkan, {
    output$unduh_rancangan <- renderUI({
      downloadButton(
        outputId = "simpan_grafik",
        label = "Simpan Grafik"
      )
    })
  })
  
  output$simpan_grafik <- downloadHandler(
    filename = function() {
      paste0("Grafik ", input$jenis_grafik,".png")
    },
    content = function(file) {
      ggsave(file, plot = grafik(), device = "png")
    }
  )
  
}