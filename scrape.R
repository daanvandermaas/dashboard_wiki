scrape = function(){
library(rvest)

#Vind alle pagina titels:
alle_paginas <- read_html("https://waterwegenwiki.cf-prod.intranet.rws.nl/index.php?title=Speciaal:AllePaginas")

alle_paginas = html_attr(html_nodes(alle_paginas, "a"), "href")[-1]
alle_paginas = alle_paginas[ !alle_paginas  %in% c("https://waterwegenwiki.cf-prod.intranet.rws.nl/index.php?title=Speciaal:AllePaginas", "https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Contents", "/index.php?title=WaterWegenWiki:Algemeen_voorbehoud", "//www.mediawiki.org/",  "/index.php?title=WaterWegenWiki:Privacybeleid", "/index.php?title=WaterWegenWiki:Info") ]



#loop door alle pagina titels hen

tags_per_pagina = list()

for(i  in 1:length(alle_paginas)){
  titel = alle_paginas[i]
  link = paste0('https://waterwegenwiki.cf-prod.intranet.rws.nl' , titel)
  print(link)
  pagina = read_html(link)
 
  
  
  if(  0 < sum( html_text(html_nodes(pagina, 'h1'))  ==   "Raportage tags[bewerken]"  )){
    
  pagina = html_text(pagina)
  
  pagina = strsplit(pagina, "Raportage tags")[[1]][3]
  pagina = strsplit(pagina, '\t')[[1]][1]
  pagina = strsplit(pagina, '\n')[[1]][2]

  pagina = strsplit(pagina, '\\.')[[1]]
 
  tags_per_pagina[[i]] = pagina
    
  }else{
    tags_per_pagina[[i]] = list()
  }
  
  
  
   
}


#geef namen en filter alle paginas zonder raportage eruit
names(tags_per_pagina) = alle_paginas


tags_per_pagina = lapply(tags_per_pagina, function(x){
  if(length(x) == 0){
    return(NA)
  }else{
    return(x)
  }
  
} )

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

tags_per_pagina <- na.omit.list(tags_per_pagina)

return(tags_per_pagina)
}