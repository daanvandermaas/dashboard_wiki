lees_tabel = function(file_name){
  
  
  tabel = read.csv(file_name)
  
  colnames(tabel) = lapply(colnames(tabel) , function(x){
    if(grepl(x = x, pattern = '\\.')){
      strsplit(x = x , '\\.')[[1]][2]
    }else{
      x
    }
    
  })
  return(tabel)
}