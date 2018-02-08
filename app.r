library(shiny)

createLink <- function(val) {
  sprintf("<a href=\"%s\" target=\"_blank\" class=\"btn btn-primary\">Info</a>",val)
}





tabel = read.csv('db/projecten_scrape1.csv')

onderwerpen = c("Scope", 
                "Tools",
                "Budget", 
                "Opdrachtgever",
                "Kennisveld")





ui <- fluidPage(
  titlePanel("Dashboard waterwegenwiki"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      
      selectInput(inputId = "var", 
                  label = "Wat wil je plotten?",
                  choices = onderwerpen),
      
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      dataTableOutput('tabel')
    )
  )
  )

  


server <- function(input, output) {
  
  
  #output complete tabel
  output$tabel <- renderDataTable({ 
   tabel = tabel[order(tabel[input$var]),]
   
 
   
   
   tabel$active_link_project = createLink(tabel$link)
   tabel$active_link_kennisveld = createLink(tabel$Link.kennisveld)
   return(tabel[, !colnames(tabel) %in% c('link', 'Link.kennisveld') ] )
  }, escape = FALSE)
  
  
  
  #plot
  
  output$distPlot <- renderPlot({
    
   
    tabel = tabel[order(tabel[input$var]),]
    
    
    
    
    tabel$active_link_project = createLink(tabel$link)
    tabel$active_link_kennisveld = createLink(tabel$Link.kennisveld)
    
    
    
    
    
    x = unlist( sapply( as.character( tabel[,input$var]) , function(x){
      strsplit(x = x, ',') 
    }))
    x = sapply( x , function(x){
      gsub(x = x, pattern = ' ', replacement = '') 
    })
    names(x) = NULL
    
    
    
    x = as.matrix(table(x))
    
    
    barplot( x[,1], names.arg = rownames(x), col = "#75AADB", border = "white", xaxt="n")
    
    
    text( ( seq(0, nrow(x) - 1, by=1) +0.3) *  (par("usr")[2] - par("usr")[1])/nrow(x)  , par("usr")[3] - 0.4, labels = rownames(x), srt = 45, pos = 1, xpd = TRUE)
    
    
    
    
  })
  
 
   
   
}


shinyApp(ui, server)

#library(rsconnect)
#rsconnect::deployApp()










