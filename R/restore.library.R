

restore.library<-function(minutes = -Inf, date='1970-01-01')
  {
  
  #1 Stop if both minutes and deates were set 
    if (minutes!=-Inf & date!='1970-01-01') {
      msg <- "You may set *either* the minutes or the date arguments, but ot both"
      message(msg)
      exit()
    }
  
  #1.5 Set minutes based on date, if set
    if (date!='1970-01-01')
    {
      minutes <- as.numeric(difftime(Sys.time(),date,units='mins'))
    }
  
  #2 Default minutes,switch to minutes since 1970 01 01
    if (minutes==-Inf) minutes = as.numeric(difftime(Sys.time(), as.Date("1970-01-01"),units='mins'))
  
  #3 See all the packages that will be restored
    purged.df <- read.local.rds("purged.rds")
    
  #4 If none, return
    if (nrow(purged.df)==0) {
      msg  <-  "We have no record of packages removed by groundhog; nothing to restore."
      message(msg)
      return(invisible(TRUE))
    }
    
  #5 If some found, check whether with the subset there are some
    subset <- purged.df$time > as.numeric(Sys.time()-minutes*60)
    purged.df_subset <- purged.df[subset,]
    
    
  #6 There is nothing n the subset
      if (nrow(purged.df_subset)==0)
      {
        msg = "We have no record of packages removed by groundhog"
        if (date!='1970-01-01') msg=paste0(msg, " since ",date,". There is nothing to restore.")
        message(msg)
        exit()
      }
    
  #7 Inform what would be restored
      if (nrow(purged.df_subset)>0)
      {
        msg <- paste0("groundhog says: There are " , nrow(purged.df_subset) , " packages that can be restored.\n",
                     "To proceed and restore them, type 'restore', to stop type anything else.")
        
        message1(msg)
        answer <-readline(prompt="  >")
        if (tolower(answer) != 'restore') {
          
          message("You typed '",answer,"', packages were not restored.")
          return(invisible(FALSE))
        }
        
  #8 Restore #k=1
        if (tolower(answer) == 'restore') {
          
          message1("Will restore ", nrow(purged.df_subset), " package.")
          reinstalled.pkg_vrs = c()
          for (k in 1:nrow(purged.df_subset))
          {
          #8.1 Setup paths 
              pkg_vrs <- purged.df_subset$pkg_vrs[k]
              pkg <-get.pkg(pkg_vrs)
              vrs <-get.vrs(pkg_vrs)
              installation.path <- get.installed_path(pkg,vrs)
              from <- paste0(installation.path , "/" ,pkg)
              to   <- .libPaths()[1] 
              old  <- paste0(to,"/",pkg)
              
          #8.2 Purge destination package if it exists 
              if (file.exists(old)) {
                random <- paste0(sample(letters,size=6),collapse = '')
                new <- paste0(old , "_",random,"_PURGE")  #add 6 random letters and _PURGE
                purged   <- file.rename(old , new)
              } #ENd purge
                
          #8.3 Copy from groundhog to local
              #Message
                message1("Re-installing " , k , " of " , nrow(purged.df_subset), ":  ",basename(installation.path))
                
              #Paths
                from  <- paste0(installation.path , "/" , pkg)
                to    <- .libPaths()[1] 
                
              #Copy
                copy.outcome <- file.copy(from , to, recursive = TRUE)    
                
              #If success add to vector
                if (copy.outcome==TRUE) reinstalled.pkg_vrs <- c(reinstalled.pkg_vrs, pkg_vrs)
      
          } #ENd for loop over re-installing pkgs
          
          
          
          #9 Update purged, dropping all the re-installed pkgs
            purged.df <- purged.df[!purged.df$pkg_vrs %in% reinstalled.pkg_vrs,]
            save.local.rds(purged.df, 'purged.rds')
          
            
          #10 Message
            msg = paste0("=========================================================\n",
                         "IMPORTANT\n",
                         "   To finalize the re-installation you need to:\n",
                         "       1) Restart the R session (in R Studio CMD/SHFT-CTRL-F10)\n",
                         "       2) Run `library('groundhog')` again.\n\n",
                         "  Only when you do (2) will temporary files that can create conflicts\n",
                         "  be deleted. If you do not do (2), packages may behave erratically.\n\n",
                         "         ***   Restart R Session  ***")
                         infinite.prompt(msg,valid_answers='uncle',must.restart = TRUE)
            
      } #IF they answered 'restore
      
    } #If we found some packages to restore
     
  
  } #End of `restore` function