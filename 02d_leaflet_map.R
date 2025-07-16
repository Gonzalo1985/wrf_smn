# Capturar click en el mapa
cds <- reactiveVal(NULL)

click <- eventReactive(input$leaflet_ith_click, {
  click <- input$leaflet_ith_click
  latlon <- c(lat = click$lat, lon = click$lng)
  cds(latlon)
  latlon
})

data.leaf <- eventReactive(input$Calcular_ith, {
  ffp <- file.for.points()
  data <- load.netcdf.terra(nc.filenames = ffp[2], variable = "T2")
  
})

output$leaflet_ith <- renderLeaflet({
  draw.map(x = data.leaf()[[4]]$x, y = data.leaf()[[4]]$y)
})