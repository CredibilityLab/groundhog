

  install.one<-function(url)
  {
     #Remote
      remote <- 0
      if (regexpr('remote::', url)>0 ) remote <- 1
      
      if (remote==0) install.one.source(url)
      if (remote==1) install.one.remote(url)
  
  }


  install.one.source <- function(url)
  {
     #paths
      log_path         <- paste0(get.groundhog.folder(),"/batch_installation_log.txt")
      dir.create(dirname(log_path),recursive = TRUE,showWarnings = FALSE)
            
     #File & package
      filename <- basename(url)
      pkg_vrs <- gsub(".tar.gz", '', filename)  #remote extension tar.gz
      vrs <- get.vrs(pkg_vrs)
      pkg <- get.pkg(pkg_vrs)
      
    #Installation location
      installation_path <- get.pkg_search_paths(pkg,vrs)
      dir.create(installation_path,recursive=TRUE, showWarnings = FALSE)

    #Log attempt to install
      t1 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      write(paste0(t1, " - Attempting to install ",pkg_vrs," from ",url),log_path,append = TRUE)
      
    #Install
      install.packages(url,type='source',repos=NULL, dependencies=FALSE,lib=installation_path)
      
    #Log success
      ip <- installed.packages(installation_path)
      t2 <- format(Sys.time(), "%Y-%m-%d %x")

      if (nrow(ip)>0)   write(paste0(t2," - Succeeded installing ",pkg_vrs),log_path,append=TRUE)
      if (nrow(ip)==0)  write(paste0(t2," - FAILED! installing ",pkg_vrs),log_path,append=TRUE)
      } 
      
      
      
  
  
  
  install.one.remote<-function(url)
    {
        #install.source() encodes the info for the clone with this syntax
                #url=remote::usr::pkg::date
        
      #parse the 'url'
      
          parts <- strsplit(url,"::")
          remote_id= parts[[1]][2]
          usr      = parts[[1]][3]
          pkg      = parts[[1]][4]
          date     = parts[[1]][5]
          sha      = parts[[1]][6]
    
    
    
         #Load remotes
           load.pkg_utility('remotes',date)

          #Location where the clone is
            clone_path <-        get.clone_path(pkg, usr , remote_id)                      #see remote_functions.R
            installation_path <- get.installation_path_remote(pkg , date, remote_id, usr)  #see remote_functions.R
                   
          #Install it 
            try_install_git(path=clone_path,  dependencies = FALSE , lib=installation_path, ref=sha, INSTALL_opts = '--no-lock')
                     #Function #8 in remote_functions.R (tries with and without file)
    
    }
  
