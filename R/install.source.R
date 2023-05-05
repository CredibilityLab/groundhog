

  install.source <- function(snowball,date,cores)
  {
    
    #Restart the feedback data.frame  so that if they install something new it starts from scratch
      on.exit(.pkgenv[['df.feedback']]<-NULL)
    
    #0 Message if error
        message_with_solutions <- 
                          paste("Possible solutions:\n",
                                "  1) First, simply try again, the error may be a fluke.\n",
                                "  2) Inspect console log, you could be missing non-CRAN dependencies\n",
                                "     (e.g., RTools for Windows, XQuartz for Mac, libssl for Ubuntu),\n",
                                "     or have a connection problem, or another compatibility problem.\n",
                                "  3) If the error indicates that a dependency is missing, re-run the\n",
                                "     groundhog.library() call with option `force.install=TRUE`\n",
                                "  4) If the package that failed is a dependency for a package you want,\n",
                                "     you may install another version of that failed dependency directly\n",
                                "     with a groundhog.library() call, just for that failing pkg, for a \n",
                                "     different date. Then re-run this groundhog.library() call adding the\n",
								                "     `ignore.deps` argument, to allowing the version mismatch for that pkg.\n",
								                "     Use `toc(<pkg>)` to find out version release dates of 'pkg'.\n",
								                "  5) Similar to (4), use toc(<pkg>) to find the date for the next version\n",
								                "     of the failing pkg and use a date after it for this entire\n",
								                "     groundhog.library() call.\n",
								                "  6) Visit http://groundhogr.com/troubleshooting")
                 
    
    #1 Keep only source  packages that are not yet installed
        snowball <- snowball[snowball$from  %in% c('source', 'github','gitlab') & snowball$installed==FALSE, ]
        
    #1.5 early return if nothing to install
          snowball$success = snowball$installed
          if (nrow(snowball)==0) return(snowball)
        
    #2 Get URL for installing each source
        
        #2.1 Source files available as current pkgs (not in the archive)
            ap_source <- get.current.packages("source")
        
        #2.2 Basis of URL
           repos <- as.character(getOption("repos"))
           
        #2.3 Add url for source
          snowball$source_url <- ifelse(snowball$pkg_vrs %in% ap_source$pkg_vrs, 
                      paste0(repos , "/src/contrib/" ,snowball$pkg_vrs , ".tar.gz"),
                      paste0(repos , "/src/contrib/Archive/" ,snowball$pkg , "/" ,  snowball$pkg_vrs , ".tar.gz"))
      
        #2.4 Modify default URL if a package is remote
          
          #Check if snowball includes remote packages (it would have the column usr & sha)
            if ("sha" %in% names(snowball))
            {
              #Find the remotes and instead of URL provide info remote::usr_pkg to install from clone
                for (k in 1:nrow(snowball))
                {
                
                #If a sha is found, it's remote
                  if (!is.na(snowball$sha[k]))
                  {
                    #We figure out github vs gitlab from installation path  
                      remote_id <- ''
                      if  (regexpr('/_github/', snowball$installation.path[k])[[1]] > 0) remote_id <- 'github'
                      if  (regexpr('/_gitlab/', snowball$installation.path[k])[[1]] > 0) remote_id <- 'gitlab'
                      
                    #Then provide the 'url' (a ";;" separated string with all the fields needed to provide clone installation instructions)
                      snowball$source_url <- ifelse(!is.na(snowball$sha), 
                                                  paste0("remote::",remote_id,"::",snowball$usr,"::",snowball$pkg,"::",date,"::",snowball$sha),
                                                  snowball$source_url)
                                                  
                      #Note: this modified URL is then read by install.one.source (install.one.R) to install from clone  
                      
                  } #End if sha found
                }   #End loop remote
            }       #End if one remote at least 
          

        #2.5 Start message
          message2("Will now install ",nrow(snowball), " packages from source")
    

      if (cores>1 & nrow(snowball)>1)
      {
        
              
          #3.0 Message  
              message1("Will rely on `",cores, "` core processors for faster in-parallel installation")
              message1("To force sequential installation set option `cores=1` in groundhog.library() ")
        
          #3.1 Get snowflakes (snowball broken into parallel installable parts)
              snowflakes <- get.snowflakes (snowball, date) #get.snowflakes.R
            
            
          #3.2 Loop over snowflakes
              
              for (k in 1:length(snowflakes))
              {  
                
            #3.4 Get snowball subset for this snowflake
                  snowball.k <- snowball[snowball$pkg %in% snowflakes[[k]], ]
                      
            #3.5 Feedback
                  try(message.batch.installation.feedback(snowball,snowflakes,k,cores)) #message.batch.installation.feedback.R
                  
                  
            #3.6 Subset the flake into 'source' vs remote
                  snowball.k_source <- snowball.k[snowball.k$from=='source',]
                  snowball.k_remote <- snowball.k[snowball.k$from!='source',]
                  n.source <-nrow(snowball.k_source)
                  n.remote <-nrow(snowball.k_remote)
                  
            #3.7 Install source in snowflake
                  if (n.source>0) {
                  utils::install.packages(snowball.k_source$source_url,repos=NULL, type='source', Ncpus=min(cores,n.source),
                                   lib=snowball.k_source$installation.path)
                  }
                  
            #3.8 Install remotes in snowflake
                  if (n.remote>0) {
                    for (rk in 1:n.remote)
                    {
                    install.one (url=snowball.k_remote$source_url[rk])  
                    }  
                  }
                  
            #3.9 Check that everything was installed.
                  ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE,row.names = NULL)
                  ip$pkg_vrs = paste0(ip$Package,"_",ip$Version)
                  snowflake.pkg_vrs <- snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ]
                  missing.pkg_vrs <-  snowflake.pkg_vrs[!snowflake.pkg_vrs %in% ip$pkg_vrs]
                  
                   
                
            #3.10 If it was not, error
                  if (length(missing.pkg_vrs)>0) {
                      message1("\nThe following package(s) from this batch failed to install:\n",pasteQC(missing.pkg_vrs))
					            message1("\n" , message_with_solutions)
                      message('\n\n                  --   Installation Failed   --  ')
                      exit()
                  }
                
            } #Loop installing snowflakes
              
          } #End parallel installation
    
          
          
    #4 Sequential installation
      if (cores==1 | nrow(snowball)==1 )
      {
          
          #4.2 Start clock for install feedback k=1
              start.time=Sys.time()
            
          #4.3 Loop over individual packages
              for (k in 1:length(snowball$source_url))
              { 
              #Feedback
                installation.feedback(k, date, snowball, start.time) 

              #Install
                install.one (url=snowball$source_url[k])       
              
              #Verify installation
                  ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE,row.names = NULL)
                  ip$pkg_vrs = paste0(ip$Package,"_",ip$Version)
                  if (!snowball$pkg_vrs[k] %in% ip$pkg_vrs) {
                    
                      message1("\nThe package '",snowball$pkg_vrs[k],"' failed to install.")
					            message1("\n" , message_with_solutions)
                      message('\n\n                  --   Installation Failed   --  ')
                      exit()
                  }
         
              }  #End install loop
      
    } #End sequential install
          
          
  }#End function install.source()