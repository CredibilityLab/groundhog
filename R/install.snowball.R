# Install snowball

#########################################################

 


  install.snowball=function(snowball, 
                            date, 
                            force.install = FALSE, 
                            force.source = FALSE, 
                            quiet.install = TRUE,
                            install.only=FALSE, 
                            skip.remotes=FALSE, 
                            recycle.files=FALSE)     {
   
    #0 Check if MRAN is down
      #Assume not
        mran.is.down <- FALSE
  
      #See if we have a text file saying it is down saved within the last 5 hours    
        mran.is.down_path <-file.path(get.groundhog.folder(),'mran.is.down.txt')
        
      #If a file exists it checks if it is less than 5 hours old, in which case we assume MRAN is down
        if (file.exists(mran.is.down_path) && as.numeric(Sys.time() - file.mtime(mran.is.down_path)) < 5*60) {
            mran.is.down <- TRUE
            
          }
    
      #If MRAN is down, install from source instead of MRAN 
        if (mran.is.down==TRUE ) {
            snowball$from <- ifelse(snowball$from=='MRAN' & snowball$installed==FALSE,'source',snowball$from)
              message1('groundhog says: MRAN seems to be down. Will install from source (much slower).')
			        message1('           --  To give MRAN another try, run `mran.is.up() --')
            }
    
        
    #1 Preliminaries
      #1.0 souce remote dummies
          source <- snowball$from=='source'
          remote <- snowball$from %in% c('gitlab','github')
          source.remote <- source | remote
     
      
      #1.1 Count number of rows
          n.snowball=nrow(snowball)
        
      #1.2 Main package
         main.pkg_vrs=snowball$pkg_vrs[n.snowball]

      #1.3 FORCE INSTALL
          if (any(snowball$installed) & force.install) {
            
            
          #Subset of packages that are installed
              snowball.installed <- snowball[snowball$installed, ]
              
          #Drop any that are base
              snowball.installed <- snowball.installed[!snowball.installed$pkg %in% base_pkg(),]
              
          # Get their path
              snowball.installed$paths <- mapply(get.installed_path, snowball.installed$pkg, snowball.installed$vrs)
          
          # Delete the paths
              unlink(snowball.installed$paths, recursive=TRUE, force = TRUE)
              
          # Reload snowball for it may be incomplete since some packages used to be installed
              snowball <- get.snowball(snowball$pkg[n.snowball], date)
          } # End #1.4

      # 1.4. FORCE SOURCE
           if (force.source || .Platform$pkgType == "source") {
            snowball$from <- ifelse(remote, snowball$from, 'source')
            }
        
      #1.5 Directory for downloaded binaries, source files, & libraries for installed packages
          temp_path <- paste0(get.groundhog.folder() ,"/temp")
          dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
          for (k in 1:nrow(snowball))
            {
            dir.create(snowball$installation.path[k], recursive = TRUE, showWarnings = FALSE)
            } 
      
        #1.6 Original selection, in for one source file we modify it temporarily, we return to this value
          quiet.install.original <- quiet.install
        
          
      #1.7 Drop remotes if install.only==TRUE
          #if (install.only==TRUE) {
          #  snowball <- snowball[!snowball$from %in% c('github','gitlab') ,]
          #}
          
          
      #1.8 CRAN URL
          r <- getOption("repos")
          CRAN.mirror.url <- as.character(r["CRAN"])
          
    #####################
    #2 CRAN
    #####################
      #2.1 Subset of CRAN packages to download
        snowball.cran <- snowball[snowball$installed==FALSE & snowball$from=="CRAN",]
        n.cran=nrow(snowball.cran)
        
        if (n.cran>0)
          {
          message2("\ngroundhog says: will now download ",n.cran, " binary packages from CRAN")
        
      #2.2 Download all CRAN binaries
          if (getRversion()>"3.3") {
            cran.binaries <- data.frame(utils::download.packages(snowball.cran$pkg, type='binary', destdir=temp_path,  method='libcurl'),stringsAsFactors = FALSE)
          } else {

            cran.binaries <- data.frame(utils::download.packages(snowball.cran$pkg, type='binary', destdir=temp_path),stringsAsFactors = FALSE)
          }
          names(cran.binaries) <- c("pkg.cran","downloaded.path")
      
          
      #2.3 Unzip them 
          message2("\ngroundhog says: all ",n.cran, " files downloaded. Now they will be installed")
        
          for (k in 1:nrow(snowball.cran)) {
            

                infile  <- as.character(cran.binaries$downloaded.path[k])
                outfile <- as.character(snowball.cran$installation.path[k])
                message1(k,") Installing: ",snowball.cran$pkg_vrs[k])
                
				
				        #2.3.1 Verify it was downloaded
  					      exit.if.download.failed(snowball.cran$pkg_vrs[k],infile)  #Function #19 in utils.r
						
                #2.3.2 Get extension
                  ext <- tools::file_ext(infile)

                
                #2.3.3 if it is a zip file, unzip it 
                  if (ext == "zip") {
                    utils::unzip(infile, exdir=outfile)
                    
                #2.3.4 Otherwise, run untar
                  } else {
                    utils::untar(infile , exdir=outfile)        
                  }

                #Note: untar() works with zip files, but errors pop up in older Windows versions
             

                #delete
                  unlink(cran.binaries$downloaded.path[k])

                } #End unzip loop
          } #End if n.cran>0
          
    ##########################
    #3 MRAN          
    ###########################

      #3.1 Subset of MRAN packages to download
        snowball.mran <- snowball[snowball$installed==FALSE & snowball$from=="MRAN",]
        n.mran <- nrow(snowball.mran)
        
      #3.1.5 If any MRAN in snowball found, install them
        if (n.mran>0)
        {  
        message2("\ngroundhog says: will now look for ",n.mran, " binary packages in MRAN (a Microsoft archive of CRAN).")
        message1("MRAN is slower than CRAN, but faster than installing from source.")
        
      #3.2 Setup URL to use as repository for each package
        repos.mran <- paste0("https://mran.microsoft.com/snapshot/", snowball.mran$MRAN.date, "/")
      
      #3.3 Initialize dataframe that will store all results
          mran.binaries <- data.frame(pkg.mran=character() ,  downloaded.path=character(), stringsAsFactors = FALSE)
        
      #3.4 Loop downloading
          good.mran.file <- c()
          
          j1 <- 0 #Counter for actually downloaded files
          
          for (k in 1:n.mran) {
        
    #3.4.5 Download if we are not recylcing or the file is not here
    mran.path.k <- file.path(temp_path, snowball.mran$pkg[k])
    if (recycle.files==FALSE || (recycle.files==TRUE & !file.exists(mran.path.k)) )
    {              
              
                
            #Dummy to identify if a problem is found and move file to source
              good.mran.file[k] <- TRUE   
            
      #3.5 Verify the binary being served is the one we want
            #Get the available packages on that date
              ap <- utils::available.packages(utils::contrib.url (repos.mran[k],'binary'))
              
          #Check MRAN down
            #If ap is empty we did not get any packages from MRAN, it may be down, save the file taht tells groundhog
              #not to try mran again within 5 hours 
              if (nrow(ap)==0) {
                  
                  #Save file that indicated mran is down for the next five hours
                    write(Sys.time(),mran.is.down_path)
                  
                  #Message
                    message("\n\n\nGroundhog says:")
                    message("We were unable to connect to the MRAN server. We won't attempt to connect again for the next 5 hours.")
                    message("   ---  Please rerun the groundhog.library() command you just run and we will install from source instead  ---")
                    exit()
              }#End MRAN
                  
              
            #Format available packages as data.frame
                ap.df <- data.frame(ap, stringsAsFactors = FALSE) 
              
            #Get row with target pkg
                ap.pkg <- ap.df[ap.df$Package==snowball.mran$pkg[k],]
                
            
            #If there is a match for that pkg_vrs, get it
              if (nrow(ap.pkg)>0 && ap.pkg$Version == snowball.mran$vrs[k])
              {
            #Add to downloaded counter
              j1 <- j1 + 1
            #Message
              message1(j1,") Downloading: '",snowball.mran$pkg_vrs[k],"' from MRAN")
    
            #Download it from MRAN
              #Is R being used newer than 3.2.0?
                if (getRversion()>"3.3") {
                    mran.binaries_rowk <- utils::download.packages(snowball.mran$pkg[k], type='binary',repos = repos.mran[k],available=ap, destdir=temp_path, method='libcurl')
                  } else {
                #IF R is 3.2 or older
                    mran.binaries_rowk <- utils::download.packages(snowball.mran$pkg[k], type='binary',repos = repos.mran[k],available=ap, destdir=temp_path)
                  }
                  
              
            
            #If file was successfully downloaded
                  if (nrow(mran.binaries_rowk)==1) {
                      mran.binaries[k,] <-mran.binaries_rowk  
            #If file did not download 
                  } else {
                   #Make not that this file did not succeed, and let's say MRAN is down
                    good.mran.file[k] <- FALSE 
                    mran.is.down      <- TRUE
                    write(Sys.time(),mran.is.down_path)
                  } 
            #IF file was not the right version
              } else {
                #If not found, then it is a bad mran pkg
                good.mran.file[k] <- FALSE
                snowball$from[snowball$pkg==snowball.mran$pkg[k]] <- 'source'
                
              }
                
       } #End if recycling
                
      } #End loop over MRAN binaries

                  
      #3.4 Unzip them 
        #Message
          if (j1 >  0) message2("groundhog says: found ",j1," packages in MRAN, will install them now.")
          if (j1 == 0) message2("groundhog says: did not find any packages in MRAN")
      
        #Reset counter
          j2 <- 0 
        

        for (k in 1:nrow(snowball.mran)) {

            
          #Correct MRAN, unzip
              if (good.mran.file[k])
              {
                  j2 <- j2 + 1
              #Legacy check, from before using available.packages() to ensure correct MRAN file was downloaded
              #Kept as an extra security, but no mismatched file should be downloaded so no mismatched file should be found
               pos <-  regexpr(snowball.mran$pkg_vrs[k], mran.binaries$downloaded.path[k]) 
              if (pos>0 ) {
                #message 
                  message1(j2,") Installing: '",snowball.mran$pkg_vrs[k],"'")
                
                #Get extension of downloaded files
                 ext <- tools::file_ext(mran.binaries$downloaded.path[k])

                #if it is a zip file, unzip it 
                  if (ext=="zip") {
                        utils::unzip(mran.binaries$downloaded.path[k] , exdir=snowball.mran$installation.path[k])
                #Otherwise, run untar
                  } else {
                  utils::untar(mran.binaries$downloaded.path[k] , exdir=snowball.mran$installation.path[k])        
                  }
                #delete
                  if (recycle.files==FALSE) unlink(mran.binaries$downloaded.path[k])
                  } else { 
                #if the name of the file does not match despite passing available.packages check, bad file
                      good.mran.file[k] <- FALSE
                }#End if pos>0
              }#End if good mran file  
              
          #Incorrect MRAN, put it up for source
              if (!good.mran.file[k]) # don't use else{} because prior if() could chnage the value
                  {
                
                  #Update snowball to get this package from source instead
                      sk=match(snowball.mran$pkg_vrs[k],snowball$pkg_vrs)  #package number in snowball
                      snowball$from[k]="source"
                  } #End if !good.mran
              }#End loop over mran
          message1() #skip a line for next message

        } #End if any MRAN files found


  ##################################################################################################
    #4 LOOP OVER SNOWBALL INSTALLING SOURCE & REMOTE IF ANY,  ADDING TO LIBPATH and loading
        
      #4.0 update sourre T/F vector,  in case a failed installations became source for backup
        source <- snowball$from == 'source'
        
      #4.0.5 workaround, if base say it is installed
          snowball$installed <- ifelse(snowball$pkg %in% base_pkg(),TRUE, snowball$installed)
        
              #Base packages would re-appear as not installed when force.install=TRUE
              #This hard codes them bck to TRUE
          
      #4.1 Any Source or Remote files remain to be installed?
        
        
          n.source <- sum(source & snowball$installed==FALSE )
          n.remote <- sum(remote & snowball$installed==FALSE )
          
          if (skip.remotes==TRUE) n.remote <-0
          
          if (n.source + n.remote > 0) {
            
            
      #4.2 Start clock for install feedback
             start.time=Sys.time()
              
      #4.3 Show message
            if (n.source>0 & n.remote==0) message1("Will now attempt installing ",n.source," packages from source.")
            if (n.source>0 & n.remote> 0) message1("Will now attempt installing ",n.source," packages from source and ",n.remote," from remote repositories (e.g., GitHub, gitlab).")
            if (n.source==0 & n.remote> 0) message1("Will now attempt installing ",n.remote," packages from a git repository (GitHub or GitLab).")

      #4.4 Smaller snowball to send to feedback
            snowball.source <- snowball[(remote | source) & snowball$installed==FALSE,]
            
      #4.5 Counter for snowball source
            k.source_remote <- 1
            
      #4.6 Load list of *current* SOURCE packages
            ap_source <- get.current.packages("source")
            
          }      
        
      #4.7 Loop through entire snowball: loading CRAN/MRAN and installing SOURCE

          for (k in 1:n.snowball)
          {
        
            
        #4.8 Install source 
            if (snowball$from[k]=='source' & snowball$installed[k]==FALSE )
              {
              
       
        #4.9 If needed version is there, get source from current
                if (snowball$pkg_vrs[k] %in% ap_source$pkg_vrs) 
                  {
                  #If it is current, get from page with all sources
                    url <- paste0(CRAN.mirror.url , "src/contrib/" ,snowball$pkg_vrs[k] , ".tar.gz")
                    } else { 
                  #If it is not current, use archive
                    url <- paste0(CRAN.mirror.url , "src/contrib/Archive/" ,snowball$pkg[k] , "/" ,  snowball$pkg_vrs[k] , ".tar.gz")
                  }
                
        #4.10 Feedback on time to user
                installation.feedback(k.source_remote, date, snowball.source, start.time) 
                
              #Add to counter for feedback 
                k.source_remote <- k.source_remote+1
                
                
        #4.11 Bypass quiet install for slow packages
                seconds <- snowball$installation.time[k]
                minutes <- round(seconds/60,0)
                
               
                if (snowball$installation.time[k]>120 & quiet.install==TRUE) {
                      message("### PATIENCE WARNING ###\nThe next package is expected to take about ",minutes," minutes to install.\n ",
                        "Usually groundhog supresses all installation output that floods this console, but for long to install packages ",
                        "it is included. See you in about ",minutes," minutes.")
                      quiet.install <- FALSE
                      Sys.sleep(10)
                      }
                
              #Install source 
                              #(turn off warnings because if something goes wrong, we try again, with warnings on)
                              #this prevents a package that actually installed successfully on the 2nd attempt, showing a warning)
                if (quiet.install==TRUE) options(warn=-1)
                      if (getRversion()>"3.3") {
                          utils::install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", dependencies = FALSE, quiet = quiet.install, method='libcurl', INSTALL_opts = '--no-lock')
                      } else {
                          utils::install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", dependencies = FALSE, quiet = quiet.install, INSTALL_opts = '--no-lock')
                      }
                if (quiet.install==TRUE) options(warn=0)
              } #End if source
            
                 
        #4.12 Return quiet install to original selection   
                quiet.install <- quiet.install.original
         
        #4.13 If it did not install and was quietly installing from source, try again not quietly
            if (!is.pkg_vrs.installed(snowball$pkg[k], snowball$vrs[k]) & 
                snowball$from[k]=="source" & 
                quiet.install==TRUE)
                  {
                  message1("Will try again, now showing all installation output.")
                  utils::install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", 
                                      dependencies = FALSE, quiet = FALSE, INSTALL_opts = '--no-lock')
                  }
            
            
            
        #4.14 If not installed show error (the check skips github package because its location is not based on pkg_vrs and we check it manually in the github addon script)
              if (!is.pkg_vrs.installed(snowball$pkg[k], snowball$vrs[k]) & !remote[k])
              {
                  message("The package '",snowball$pkg_vrs[k],"' failed to install!")
                
                  #4.15  R TOOLS CHECK
                      if (.Platform$OS.type == "windows" & Sys.which("make") == "") {
                      message2()
                      message1(
                              "***RTOOLS ALERT***\nYou need 'R Tools' to install packages from source in Windows, but R Tools was not ",
                              "found. For help see:\nhttp://groundhogr.com/rtools"
                               )
                            } # End of if make=="
                
                
                  #4.16  R mismatch
                     rv <- r.version.check(date)
                     if (rv$r.using.majmin!= rv$r.need.majmin)
                        {
                          message2()
                          message1(
                          "The package may have failed to install because you are using R-", rv$r.using.full,"\n",
                          "which is at least one major update after the date you entered '", date, "'.\n",
                          "You can try using a more recent date in your groundhog.library() command, \n",
                          "or run it with the same date using 'R-", rv$r.need.full, "'\n", 
                          "Instructions for running older versions of R: ",
                          "    http://groundhogr.com/many")
                          }
                          
                
                #4.17 Stop the script
                    message("\n\n\n----------------   The package ", main.pkg_vrs, " did NOT install.  Read above for details  -----------------")
                    exit()
                    }
                
                
        #5 IF it is remote
                
            if (remote[k]==TRUE & snowball$installed[k]==FALSE  & skip.remotes==FALSE)
            {
              
              #Load remotes
                load.pkg_utility('remotes',date)

              #Location where the clone is
                clone_path <- get.clone_path(pkg=snowball$pkg[k], usr=snowball$usr[k], remote_id=snowball$from[k]) 
              
              #Add "file://" if it is a unix machine so that install_git will treat it as a URL
                if(.Platform$OS.type == "unix") clone_path <- paste0("file://",clone_path)
                
              #Install it
                  remotes::install_git(clone_path,  dependencies = FALSE , lib=snowball$installation.path[k], ref=snowball$sha[k], INSTALL_opts = '--no-lock')
                     #ref is the sha indicating which version is installed
            }
                
                
        #6 Add remote to remotes.df
            if (remote[k]==TRUE)
              {
              row_remotes.df <- data.frame(pkg=snowball$pkg[k],date=date,attached=FALSE,stringsAsFactors = FALSE)
              .pkgenv[['remotes.df']] <- rbind(.pkgenv[['remotes.df']], row_remotes.df)
              }
                
        #7 Verify package exists (if not base)
              if (!snowball$pkg[k] %in% base_pkg())
              {
                #Get available packages in this subfolder of groundhog.folder() with the path to package
                  ap1 <- data.frame(utils::installed.packages( snowball$installation.path[k]), stringsAsFactors=FALSE)
                
                #If less than 1 package, die
                  if (nrow(ap1) < 1) {
                      message("\n\n ---- Groundhog Says, Error: The package '", snowball$pkg[k] , "' failed to install --- ")
                  exit()
                }
              }
              
        #8 Load the package
           if (install.only==FALSE) {
             loadNamespace(package=snowball$pkg[k], lib.loc =snowball$installation.path)
             .pkgenv[['groundhog_loaded_pkgs']] <- c(.pkgenv[['groundhog_loaded_pkgs']] , snowball$pkg_vrs[k])
           }
            
      } #End loop over snowball        
          
      

  } #end of function

  
