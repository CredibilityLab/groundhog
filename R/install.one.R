
#Called for parallel loop installing source or remote
  install.one<-function(url)
  {
     #Remote
      remote <- 0
      if (regexpr('remote::', url)>0 ) remote <- 1
      
      if (remote==0) install.one.source(url)
      if (remote==1) install.one.remote(url)
  
  }
#---------------------------------
#SOURCE
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

    #Log attempt to install
      t1 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      
      
    #Install (turning off warnings since we have our own message feedback)
      warn_before <- getOption("warn") 
      options(warn = -1) 
      utils::install.packages(url,type='source',repos=NULL, dependencies=FALSE,lib=installation_path, Ncpus=1)
      options(warn = warn_before) 
      
    #Log success
      ip <- utils::installed.packages(installation_path)
      t2 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
		} 
      
   
#---------------------------------
#REMOTE

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
  
            
          #Log success 
              ip <- utils::installed.packages(installation_path)
              t2 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

              if (nrow(ip)==0)  {
              gstop(paste0("Installation of '",pkg,"' failed. \n",
                             "Sometimes simply running groundhog.library() again will work.\n",
                             "But check out any output in the console with possible indications of what went wrongn."))
              }
    }
  
