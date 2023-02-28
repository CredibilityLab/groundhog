#as of 2023 02 26 this is not running succesfully, the files downloaded from wasabi do not unzip correctly lead to empty folder


#########################################################

 
  install.snowball=function(snowball, 
                            date, 
                            force.install = FALSE, 
                            force.source = FALSE, 
                            quiet.install = TRUE,
                            install.only=FALSE)     {
   
   
        
    #1 Preliminaries
      #1.0 source|remote dummies
          source <- snowball$from=='source'
          remote <- snowball$from %in% c('gitlab','github')
          source.remote <- source | remote
     
      
      #1.1 Count number of rows
          n.snowball <- nrow(snowball)
        
      #1.2 Main package
         main.pkg_vrs <- snowball$pkg_vrs[n.snowball]

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
        
          
    
          
      #1.8 CRAN URL
          r <- getOption("repos")
          CRAN.mirror.url <- as.character(r["CRAN"])
          
          
    
          
    #####################
    #2 CRAN
    #####################
      #2.1 Subset of CRAN packages to download
        snowball.cran <- snowball[snowball$installed==FALSE & snowball$from=="CRAN",]
        n.cran <- nrow(snowball.cran)
        
        if (n.cran>0)
          {
          message2("\ngroundhog says: will now download ",n.cran, " binary packages from CRAN")
        
      #2.2 Download all CRAN binaries
          if (getRversion()>"3.4") {
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
#3 GRAN          
###########################

      #3.1 Subset of GRAN packages to download
        snowball.gran <- snowball[snowball$installed==FALSE & snowball$from=="GRAN",]
        n.gran <- nrow(snowball.gran)
        
      #3.2 If any gran in snowball found, install them
        if (n.gran>0)
        {  
        message2("\ngroundhog says: will now look for ",n.gran, " binary packages in GRAN (groundhog's archive of CRAN).")

      #3.3 URLs to download from GRAN
          #OS
            os <- get.os()
          #R Version
            r.version    <- get.r.majmin()

          #Extension
            if (os=='windows') ext <- 'zip'
            if (os!='windows') ext <- 'tgz'
          
          #URL for Binary on GRAN
            url.wasabi <- paste0("http://gran.groundhogr.com/", os , "/", r.version, "/", snowball.gran$GRAN.date, "/" , snowball.gran$pkg_vrs, ".", ext)
          
         
      #3.4 Loop downloading
          for (k in 1:n.gran) {
            
          #Message
            message1(j1," of ", n.gran , ") Downloading: '",snowball.gran$pkg_vrs[k], " from GRAN")
    
          #Download from gran
            #R > 3.3
               if (getRversion()>"3.3") {
                  dl <- try(utils::download.file(url.wasabi[j1] , file.path(temp_path, basename(url.wasabi[j1])) , mode = "wb", method = "libcurl" ))
                }
            
             #R < 3.4
               if (getRversion()<"3.4") {
                  dl <- try(utils::download.file(url.wasabi[j1] , file.path(temp_path, basename(url.wasabi[j1])) , mode = "wb" ))
                } 
              
          } #End loop over gran binaries

                  
      #3.5 Find downloaded zips, and unzip them  
          all.zip <- list.files(temp_path,full.names = TRUE)
          n.zip <- length(all.zip)
          
        #Message
          if (n.zip >  0) message2("groundhog says: found ",j1," packages in GRAN, will install them now.")
          if (n.zip == 0) message2("groundhog says: did not find any packages in GRAN")
      
      #3.6 Unzip all files found
        for (k.zip in 1:n.zip)
        {
          #Short name
            zk <- all.zip[k.zip]

          #Extension
            ext <- tools::file_ext(zk)
  
          #Find installation path in snowball
            pkg.k      <- get.pkg(basename(zk))
            k.snowball <- match(pkg.k , snowball$pkg)
            outfile    <- snowball$installation.path[k.snowball]
          
            message1('Installing ',pkg.k)
          
          #Unzip  
            if (ext=="zip") utils::unzip(zk, exdir=outfile)
            if (ext!="zip") utils::untar(zk, exdir=outfile)        
            
          #Delete zip 
            unlink(zk)
          
        }

          
      #3.7 Any binary which was not found add to snowball
          snowball.binary = snowball[snowball$from %in% c("CRAN","GRAN"),]
          failed.binary = snowball.binary$   


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
        
      #4.7 Loop through entire snowball: loading CRAN/gran and installing SOURCE

          for (k in 1:n.snowball)
          {
        
            
        #4.8 Install source 
            if (snowball$from[k]=='source' & snowball$installed[k]==FALSE )
              {
              
       
        #4.9 If needed version is there, get source from current
                if (snowball$pkg_vrs[k] %in% ap_source$pkg_vrs) 
                  {
                  #If it is current, get from page with all sources
                    url <- paste0(CRAN.mirror.url , "/src/contrib/" ,snowball$pkg_vrs[k] , ".tar.gz")
                    } else { 
                  #If it is not current, use archive
                    url <- paste0(CRAN.mirror.url , "/src/contrib/Archive/" ,snowball$pkg[k] , "/" ,  snowball$pkg_vrs[k] , ".tar.gz")
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
                      if (getRversion()>"3.4") {
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
                      msg <-  paste0(
                              "groundhog says: you need 'R Tools' to install packages from source in Windows, but R Tools was not ",
                              "found. This is an R rather than a groundhog requirement. Sometimes even after you install R Tools, ",
                              "R does not find it. You can get some help here: https://groundhogr.com/rtools \n ",
                              "A solution that tends to work is to run a line of code that updates the file that R consults when loading ",
                              "up, adding the R tools folder to it, so that it finds it. For example, if you recently installed ",
                              "R Tools in folder `c:/RTools_3.5`, then you would run this line of code: \n \n ",
                              "Sys.setenv(PATH = paste('C:/Rtools_3.5/mingw_64/bin', Sys.getenv('PATH'), sep=';')) \n \n",
                              "Please type 'OK' to confirm you have read this message.")
                          
                      infinite.prompt(format.msg(msg,header='R TOOLS ALERT!',width=80), 'ok')
                               
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
                          "Instructions for running older versions of R: \n",
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
                #if (.Platform$OS.type == "unix" & Sys.info()['sysname']!="Darwin") {
                 # clone_path <- paste0("file://",clone_path)
                  #}
                
              #Install it
                    try_install_git(path=clone_path,  dependencies = FALSE , lib=snowball$installation.path[k], ref=snowball$sha[k], INSTALL_opts = '--no-lock')
                    
                     #ref is the sha indicating which version is installed
                     #try_install_git runs remotes::install_git first on the path, and if it does not work on file://path
                     #see function #8 in remotes_functions.R
            }    

                
                
        #6 Add remote to remotes_df
            if (remote[k]==TRUE)
              {
              row.remotes_df <- data.frame(pkg=snowball$pkg[k],date=date,attached=FALSE,stringsAsFactors = FALSE)
              .pkgenv[['remotes_df']] <- rbind(.pkgenv[['remotes_df']], row.remotes_df)
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
             .pkgenv[['groundhog_loaded_pkgs']] <- c(.pkgenv[['groundhog_loaded_pkgs']] , snowball$pkg[k])
           }
            
      } #End loop over snowball        
          
      

  } #end of function

  
