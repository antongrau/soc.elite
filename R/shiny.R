graph.explorer     <- function(graph, den, layout = layout_with_fr(graph, weights = 1/E(graph)$weight)){
  
  # -----------
  # Creating data
  layout                  <- norm_coords(layout, xmin = 1, xmax = 10^10, ymin = 1, ymax = 10^10)
  vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
  colnames(layout)        <- c("X", "Y")
  data                    <- data.frame(name = V(graph)$name, X = vertex.coords[, 1], Y = vertex.coords[, 2])
  p                       <- graph.plot(graph, layout = layout, edge.color = "black", vertex.fill = "white")
  
  
  # ------------
  # UI 
  
  ui  <- shinyUI(
    fluidPage(
      titlePanel(
        "Explore"
      ), 
      fluidRow(
        column(11, plotOutput("graph.plot", click = "plot_click"))# Se hvad vi kan gøre ved størrelsen på plottet
        
      ),
      fluidRow(
        column(1),
        column(8, tableOutput("info"))
      )
      )
    )
  
  
  # ---------------
  # Server
  server <- shinyServer(function(input, output) {
    
    # Click
    
    
      output$graph.plot <- renderPlot({
      p # isolate synes ikke at forhindre at vores plot bliver opdateret
    })
    
    output$info <- renderTable({
      np <- nearPoints(data, input$plot_click, xvar = "X", yvar = "Y", threshold = 5, maxpoints = 1)
      #if(is.null(input$plot_click)) np <- data[1,]
#       out <- lapply(np$name, neighbors, graph = graph)
#       out <- lapply(out, as.matrix)
#       names(out) <- np$name
#       out <- for(i in np$name) out[[]]
#       out.mat <- do.call("rbind", out)
      
       hood <- as.matrix(neighbors(graph, v = np$name))
       colnames(hood) <- np$name
      # hood
      np
    })
    
  
  
  })
  
  
  # ---------------
  # runApp
  
  app <- list(ui = ui, server = server)
  runApp(app)
}
# library(shiny)
# data(den)
# data(pe13)
# graph <- net.elite
# soc.elite:::graph.explorer(graph, den)
