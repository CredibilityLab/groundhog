

  install.one.source <- function(url)
  {
    
     #File & package
      filename <- basename(url)
      pkg_vrs <- gsub(".tar.gz", '', filename)  #remote extension tar.gz
      vrs <- get.vrs(pkg_vrs)
      pkg <- get.pkg(pkg_vrs)
      
    #Installation location
      installation_path <- get.pkg_search_paths(pkg,vrs)
      dir.create(installation_path,recursive=TRUE, showWarnings = FALSE)

    #Install
       install.packages(url,type='source',repos=NULL, dependencies=FALSE,lib=installation_path)
    
  }
  
  
