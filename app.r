library(shiny)

source('scrape.R')
source('maak_tabel_van_scrape.r')
source('lees_tabel.r')


createLink <- function(val) {
  sprintf("<a href=\"%s\" target=\"_blank\" class=\"btn btn-primary\">Info</a>",val)
}



#tabel = read.csv('db/projecten_scrape1.csv')
file_name = 'db/projecten_scrape1.csv'
tabel = lees_tabel(file_name)
tabel = tabel[,-1]

onderwerpen = colnames(tabel)[ !colnames(tabel) %in% c('naam', 'link')]



ui <- fluidPage(
  titlePanel("Dashboard waterwegenwiki"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      
      selectInput(inputId = "var", 
                  label = "Wat wil je plotten?",
                  choices = onderwerpen),
      actionButton("button", "Scrape de wiki")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      dataTableOutput('tabel')
    )
  )
  )

  


server <- function(input, output) {
  
  tabel = lees_tabel(file_name)
  
  #output complete tabel
  output$tabel <- renderDataTable({ 
   tabel = tabel[order(tabel[input$var]),]
   
 
   
   
   tabel$link = createLink(tabel$link)
   return(tabel )
  }, escape = FALSE)
  
  
  
  #plot
  
  output$distPlot <- renderPlot({
    
   tabel = lees_tabel(file_name)
    
    
    
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
  
 
  #scrape als op knop wordt gedrukt
  observeEvent(input$button, {
scrape = scrape()
tabel = maak_tabel(scrape)
  })
   
   
}


shinyApp(ui, server)

#library(rsconnect)
#rsconnect::deployApp()



