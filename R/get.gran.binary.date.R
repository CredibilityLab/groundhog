  #1 Preliminaries
  #2 If operating system is unknown (mostly, unix) don't look for binaires
  #3 Read the gran toc
  #4 Keep the wanted version
  #5 Find date gaps (download vs wanted) and minimize
  #6 return date

get.gran.binary.date <- function(pkg_vrs , date) {

  #1 Preliminaries
    os <- get.os()
    date <- as.Date(date)


  #2 If operating system is unknown (mostly, unix) don't look for binaires
    if (os=='uknown') {
      return(as.Date('1970-01-01'))
      }
     

  #3 Read the gran toc
    gran.toc <- .pkgenv[["gran.toc"]]

  
  #4 Keep the wanted version
    #Extension
      if (os=='windows') ext='zip'
      if (os!="windows") ext='tgz'
    
    #file.wanted
      file.wanted = paste0(pkg_vrs,".",ext)
    
    #Subset
      gran.toc.sub = gran.toc[gran.toc$file == file.wanted,]  
          
    #If no date, early return
      if (nrow(gran.toc.sub)==0) return(as.Date('1970-01-01'))
    
  #5 Find date gaps (download vs wanted) and minimize
      diff <- gran.toc.sub$download.date - date
    
    #Minimizing one 
      k <- which.min(diff)
      
  #6 Return it
      return(gran.toc.sub$download.date[k])
      
}
