source('scrape.R')

scrape = scrape()

#ga na welke kolomen we hebben
kolommen = c()

for(i in 1:length(scrape)){
  
  for( j in 1:length(scrape[[i]]) ){
    kolommen = c(kolommen, strsplit( scrape[[i]][[j]], ':' )[[1]][1]    )
  }
  
}

kolommen = unique(kolommen)

kolommen = c( 'naam', kolommen, 'project_link', 'kennisveld_link' )

#maak de tabel
tabel = as.data.frame( matrix(NA, nrow = length(scrape), ncol = length(kolommen)) )
colnames(tabel) = kolommen


#vul tabel op basis van de scrape
for(i in 1:length(scrape)){
  
  for( j in 1:length(scrape[[i]]) ){
    
    tabel[i, strsplit( scrape[[i]][[j]], ':' )[[1]][1]   ] =  strsplit( scrape[[i]][[j]], ':' )[[1]][2] 
    
  }
  
  
}


#regel de namen en links


tabel$link = paste0('https://waterwegenwiki.cf-prod.intranet.rws.nl' , names(scrape) ) 


tabel$naam =  unlist( lapply( tabel$link, function(x){
  strsplit(x, '=')[[1]][2]
  
}))

write.csv(tabel, file = 'db/tabel.csv')

