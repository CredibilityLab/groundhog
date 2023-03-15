

  install.one.source <- function(url)
  {
    #log
      log_path  <- paste0(get.groundhog.folder(),"/batch_installation_log.txt")

    
     #File & package
      filename <- basename(url)
      pkg_vrs <- gsub(".tar.gz", '', filename)  #remote extension tar.gz
      vrs <- get.vrs(pkg_vrs)
      pkg <- get.pkg(pkg_vrs)
      
    #Installation location
      installation_path <- get.pkg_search_paths(pkg,vrs)
      dir.create(installation_path,recursive=TRUE, showWarnings = FALSE)

    #Log attempt to install
      t1 <- format(Sys.time(), "%Y-%m-%d %x")

      write(paste0(t1, " - Attempting to install ",pkg_vrs," from ",url),log_path,append = TRUE)
      
    #Install
      install.packages(url,type='source',repos=NULL, dependencies=FALSE,lib=installation_path)
      
    #Log success
      ip <- installed.packages(installation_path)
      t2 <- format(Sys.time(), "%Y-%m-%d %x")

      if (nrow(ip)>0)   write(paste0(t2," - Succeeded installing ",pkg_vrs),log_path,append=TRUE)
      if (nrow(ip)==0)  write(paste0(t2," - FAILED! installing ",pkg_vrs),log_path,append=TRUE)

  }
  
  
  
  install.one.remote<-
