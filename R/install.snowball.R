#' Install snowball
#'
#' Install given `snowball` from CRAN binary, MRAN binary, CRAN source or MRAN source as needed
#'
#' @inheritParams get.snowball
#' @inheritParams install.source
#' @inheritParams installation.feedback
#' @param force.install Logical (defaults to `FALSE`). If `TRUE`, even if
#'   package is found for this R-build, it will reinstall it.
#'
#' @inherit install.source return
#'
# @examples
# \dontrun{
# groundhog:::install.snowball("magrittr", "2018-02-12", include.suggests = FALSE)
# }
#'
#' @seealso [get.snowball()] to determine in which order packages are installed
#'
#' @importFrom utils install.packages
#'
#########################################################

  install.snowball=function(snowball, date, force.install = FALSE, force.source = FALSE, quiet.install = TRUE) 
    {
     #####################
    #1 Preliminaries
    #####################
    
     
      
      #1.1 Count number of rows
          n.snowball=nrow(snowball)
        
      #1.2 Main package
         main.pkg_vrs=snowball$pkg_vrs[n.snowball]

        
      #1.3 FORCE INSTALL
          if (any(snowball$installed) & force.install) {
          #Subset of packages that are installed
              snowball.installed <- snowball[snowball$installed, ]
          # Get their path
              snowball.installed$paths <- mapply(get.installed_path, snowball.installed$pkg, snowball.installed$vrs)
          # Delete the paths
              unlink(snowball.installed$paths, recursive=TRUE, force = TRUE)
              
          # Reload snowball for it may be incomplete since some packages used to be installed
              snowball <- get.snowball(snowball$pkg[n.snowball], date)
          } # End #1.4

      # 1.4. FORCE SOURCE
          if (force.source || .Platform$pkgType == "source") {
            snowball$from <- "source"
            }
        
      #1.5 Directory for downloaded binaries, source files, & libraries for installed packages
          temp_path=paste0(get.groundhog.folder()    ,"/temp")
          dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
          for (k in 1:nrow(snowball))
            {
            dir.create(snowball$installation.path[k], recursive = TRUE, showWarnings = FALSE)
            } 
      
        #1.6 Original selection, in for one source file we modify it temporarily, we return to this value
          quiet.install.original <- quiet.install
          
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

          cran.binaries <- data.frame(download.packages(snowball.cran$pkg, type='binary', destdir=temp_path))
          names(cran.binaries) <- c("pkg.cran","downloaded.path")
      
          
      #2.3 Unzip them 
          message2("\ngroundhog says: all ",n.cran, " files downloaded. Now they will be installed")
        
          for (k in 1:nrow(snowball.cran)) {
                infile  <- as.character(cran.binaries$downloaded.path[k])
                outfile <- as.character(snowball.cran$installation.path[k])
                message1(k,") Installing: ",snowball.cran$pkg_vrs[k])
                #unzip
                  untar(infile ,exdir=outfile)
                #delete
                  unlink(cran.binaries$downloaded.path[k])

                } #End unzip loop
          } #End if n.cran>0
          
    ##########################
    #3 MRAN          
    ###########################

      #3.1 Subset of MRAN packages to download
        snowball.mran <- snowball[snowball$installed==FALSE & snowball$from=="MRAN",]
        n.mran=nrow(snowball.mran)
        
      #3.1.5 If any MRAN in snowball found, install them
        if (n.mran>0)
        {  
        message2("\ngroundhog says: will now download ",n.mran, " binary packages from MRAN (a Microsoft archive storing binaries of older packages).")
        message1("MRAN is slower than CRAN for binaries, but still faster than the alternative: *source* packages from CRAN.")
        
      #3.2 Setup URL to use as repository for each package
        repos.mran = paste0("https://mran.microsoft.com/snapshot/", snowball.mran$MRAN.date, "/")
      
      
      #3.3 Initialize dataframe that will store all results
          mran.binaries=data.frame(pkg.mran=character() ,  downloaded.path=character(), stringsAsFactors = FALSE)
        
        #Loop downloading
          good.mran.file=c()
          
          for (k in 1:n.mran) {
            
          #Dummy to identify if a problem is found and move file to source
            good.mran.file[k] <- TRUE
            
          
      #3.4 Verify the binary being served is the one we want
            #Get the available packages on that date
              ap <-    available.packages(repos=repos.mran[k],type='binary')   
              ap.df <- data.frame(ap)                       
              ap.pkg <- subset(ap.df,Package==snowball.mran$pkg[k])
              
              
            #If there is a match for that pkg_vrs, get it
            if (ap.pkg$Version == snowball.mran$vrs[k])
            {
            #Message
              message1(k,") Downloading: '",snowball.mran$pkg_vrs[k],"' from MRAN")
    
            #Download it
              mran.binaries_rowk <- download.packages(snowball.mran$pkg[k], type='binary',repos=repos.mran[k],available=ap, destdir=temp_path)
            
            #If file was successfully downloaded
                  if (nrow(mran.binaries_rowk)==1) {
                      mran.binaries[k,] <-mran.binaries_rowk  
            #If file did not download 
                  } else {
                    good.mran.file <- FALSE 
                  } 
            #IF file was not the right version
            } else {
              good.mran.file[k] <- FALSE 
            } #End else for whether if binary asked for is on available.packages
              
           
            } #End loop over MRAN binaries

        
      #3.4 Unzip them 
        message2("\ngroundhog says: all ",n.mran, " files downloaded. Now they will be installed")

        for (k in 1:nrow(snowball.mran)) {
          message1(k,") Installing: '",snowball.mran$pkg_vrs[k])
            #Correct MRAN, unzip
          
              if (good.mran.file[k])
              {
              #Legacy check, from before using available.packages() to ensure correct MRAN file was downloaded
              #Kept as an extra security, but no mismatched file should be downloaed
             
              pos <-  regexpr(snowball.mran$pkg_vrs[k], mran.binaries$downloaded.path[k]) 
              if (pos>0 ) {
                #unzip
                  untar(mran.binaries$downloaded.path[k] , exdir=snowball.mran$installation.path[k])        
                #delete
                  unlink(mran.binaries$downloaded.path[k])
                  } else { 
                #if the name of the file does not match despite passing available.packages check, bad file
                      good.mran.file[k] <- FALSE
                }#EDnd if pos>0
              }#End if good mran file  
              
          #Incorrect MRAN, put it up for source
              if (!good.mran.file[k]) # don't use else{} becasue prior if() could chnage the value
                  {
                  #Tell user we will try 'source' as backup
                    message("Did not find the binary we were looking for in MRAN, will install source from CRAN instead.")
                  #Update snowball to get this package from source instead
                      sk=match(snowball.mran$pkg_vrs[k],snowball$pkg_vrs)  #package number in snowball
                      snowball$from[sk]="source"
                  } #End if !good.mran
              }#End loop over mran
          message1() #skip a line for next message

        } #End if any MRAN files found

        
        
    #################################################
    #4 INSTALL SOURCE & LOAD
    ###################################################
      
      #4.1 Any Source files remain to be installed?
          n.source=sum(snowball$from=="source" & snowball$installed==FALSE)
          if (n.source>0) {
            
          #Start clock for install feedback
              start.time=Sys.time()
              
          #Show message
            message1("groundhog says: ",n.source," packages need to be installed from source. \n",
                 "Completion time estimates are reported after each package installs.")
          
          #Smaller snowball to send to feedback
            snowball.source=snowball[snowball$from=="source" & snowball$installed==FALSE,]
            
          #Counter for snowball source
            k.source=1
            
          #Load list of *current* SOURCE packages
            ap_source=get.current.packages("source")
            
          }      
        
      #4.2 Loop through entire snowball: loading CRAN/MRAN and installing SOURCE

          for (k in 1:n.snowball)
          {
        
        #4.3 Install source 
            if (snowball$from[k]=='source' & snowball$installed[k]==FALSE )
              {
              
                    
              #If needed version is there, get source from current
                if (snowball$pkg_vrs[k] %in% ap_source$pkg_vrs) 
                  {
                  #If it is current, get from page with all sources
                    url <- paste0("https://cran.r-project.org/src/contrib/" ,snowball$pkg_vrs[k] , ".tar.gz")
                    } else { 
                  #If it is not current, use archive
                    url <- paste0( "https://cran.r-project.org/src/contrib/Archive/" ,snowball$pkg[k] , "/" ,  snowball$pkg_vrs[k] , ".tar.gz")
                  }
                
              #Feedback on time to user
                installation.feedback(k.source, date, snowball.source, start.time) 
                
              #Add to counter for feedback 
                k.source=k.source+1
                
                
              #Bypass quiet install fro slow packages
                seconds=snowball$installation.time[k]
                minutes=round(seconds/60,0)
                
                
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
                      install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", dependencies = FALSE, quiet = quiet.install, INSTALL_opts = '--no-lock')
                if (quiet.install==TRUE) options(warn=0)
              } #End if source
            
                 
            #Return quiet install to original selection   
                quiet.install <- quiet.install.original
         
        #4.4 If it did not install and was quietly installing from sourced, try again not quietly
            if (!is.pkg_vrs.installed(snowball$pkg[k], snowball$vrs[k]) & 
                snowball$from[k]=="source" & 
                quiet.install==TRUE)
                  {
                  message1("Will try again, now showing all installation output.")
                  install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", 
                                      dependencies = FALSE, quiet = FALSE, INSTALL_opts = '--no-lock')
                  }
            
            
            
        #4.5 If not installed show error    
              if (!is.pkg_vrs.installed(snowball$pkg[k], snowball$vrs[k]))
              {
                  message("The package '",snowball$pkg_vrs[k],"' failed to install!")
                
                  #4.5.1  R TOOLS CHECK
                      if (.Platform$OS.type == "windows" & Sys.which("make") == "") {
                      message2()
                      message1(
                              "***RTOOLS ALERT***\nYou need 'R Tools' to install packages from source in Windows, but R Tools was not ",
                              "found. For help see:\nhttp://groundhogr.com/rtools"
                               )
                            } # End of if make=="
                
                
                  #4.5.2  R mismatch
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
                          
                
                #4.5.3 Stop the script
                    message("\n\n\n----------------   The package ", main.pkg_vrs, " did NOT install.  Read above for details  -----------------")
                    exit()
                    }
                
                
                
        #5 Assume success at this point, load it
                .libPaths(c(.libPaths(), snowball$installation.path[k] ))
                loadNamespace(snowball$pkg[k], lib.loc =  snowball$installation.path[k]) 
          } #End loop over snowball        

  } #end of function
          
     
  
  
  