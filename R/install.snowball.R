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
    
      #1.1 Directory for downloaded binaries, source files, & libraries for installed packages
          temp_path=paste0(get.groundhog.folder()    ,"/temp")
          dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
          for (k in 1:nrow(snowball))
            {
            dir.create(snowball$installation.path[k], recursive = TRUE, showWarnings = FALSE)
            }
      
      #1.2 Count number of rows
          n.snowball=nrow(snowball)
        
      #1.3 Main package
         main.pkg_vrs=snowball$pkg_vrs[n.snowball]

        
      #1.4 FORCE INSTALL
          if (any(snowball$installed) & force.install) {
          #Subset of packages that are installed
              snowball.installed <- snowball[snowball$installed, ]
          # Get their path
              snowball.installed$paths <- mapply(get.installed_path, snowball.installed$pkg, snowball.installed$vrs)
          # Delete the paths
              unlink(snowball.installed$paths, recursive = TRUE, force = TRUE)
              snowball$installed <- FALSE
          } # End #1.4

      # 1.5. FORCE SOURCE
          if (force.source || .Platform$pkgType == "source") {
            snowball$from <- "source"
            }
        
        
      
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
          names(cran.binaries) <- c("pkg.cran","donwloaded.path")
      
      #2.3 Unzip them 
          message2("\ngroundhog says: all ",n.cran, " files downloaded. Now they will be installed")
        
          for (k in 1:nrow(snowball.cran)) {
                message1(k,") Installing: ",snowball.cran$pkg_vrs[k])
                untar(cran.binaries$donwloaded.path[k] , exdir=snowball.cran$installation.path[k])        
                
                } #End unzip loop
          } #End if n.cran>0
          
    ##########################
    #3 MRAN          
    ###########################

      #3.1 Subset of MRAN packages to download
        snowball.mran <- snowball[snowball$installed==FALSE & snowball$from=="MRAN",]
        n.mran=nrow(snowball.mran)
        
      #3.1.5 If any found, install them
        if (n.mran>0)
        {  
        message2("\ngroundhog says: will now download ",n.mran, " binary packages from MRAN (a Microsoft archive storing binaries of older packages).")
        message1("MRAN is slower than CRAN for binaries, but still faster than the alternative: *source* packages from CRAN.")
        
      #3.2 Setup URL to use as repository for each package
        repos.mran = paste0("https://mran.microsoft.com/snapshot/", snowball.mran$MRAN.date, "/")
      
      #3.3 Download all MRAN binaries
        
        #Initialize dataframe that will store all results
          mran.binaries=data.frame(pkg.mran=character() ,  donwloaded.path=character(), stringsAsFactors = FALSE)
        
        #Loop downloading
          for (k in 1:n.mran) {
            message1(k,") Downloading: '",snowball.mran$pkg_vrs[k],"' from MRAN")
            mran.binaries_rowk <- download.packages(snowball.mran$pkg[k], type='binary',repos=repos.mran[k], destdir=temp_path)
            #verify it was downloaded by checking the output is a data.frame with 1 row
                if (nrow(mran.binaries_rowk)==1) {
                  #If downloaded succesfully, add info for unzipping to the data.drame
                      mran.binaries[k,] <-mran.binaries_rowk  
                  #Else, enter NA and make it download from sour
                  } else {
                      mran.binaries[k,] <-c('missing','missing')
                      sk=match(snowball.mran$pkg_vrs[k],snowball$pkg_vrs)  #package number in snowball
                      snowball$from[sk]="source"
                  } #End if downloaded succesfully
              
            } #End loop over MRAN binaries

        
      #3.4 Unzip them 
        message2("\ngroundhog says: all ",n.mran, " files downloaded. Now they will be installed")

        for (k in 1:nrow(snowball.mran)) {
          message1(k,") Installing: '",snowball.mran$pkg_vrs[k])
          
          #Verify the right version was downloaded prior to installing (we *guess* which version MRAN has, with cran.toc.rds)
          # by checking if pkg_vrs appears in the file name
              pos <-  regexpr(snowball.mran$pkg_vrs[k], mran.binaries$donwloaded.path[k]) 
              if (pos>0) {
                  untar(mran.binaries$donwloaded.path[k] , exdir=snowball.mran$installation.path[k])        
                  
                  } else {
              #Tell user we will try 'source' as backup
                    message("Did not find the binary we were looking for in MRAN, will install source from CRAN instead.")
                
                #Update snowball to get this package from source instead
                    sk=match(snowball.mran$pkg_vrs[k],snowball$pkg_vrs)  #package number in snowball
                    snowball$from[sk]="source"
                    
                  } #End if pos<0
              }#End loop over mran
          
              message1() #skip a line for next message

        } #End if any MRAN files found

        
        
    #################################################
    #4 LOAD(CRAN,MRAN) and INSTALL SOURCE
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
            
          }      
        
      #4.2 Loop through entire snowball: loading CRAN/MRAN and installing SOURCE
          source.download.path=c()
          
          for (k in 1:n.snowball)
          {
        
        #4.3 Install source 
            if (snowball$from[k]=='source' & snowball$installed[k]==FALSE )
              {
              #URL for install
                url <- paste0("https://cran.r-project.org/package=", snowball$pkg[k], "&version=", snowball$vrs[k])
                
              #Feedback on time to user
                installation.feedback(k.source, date, snowball.source, start.time) 
                
              #Add to counter for feedback 
                k.source=k.source+1
                
              #Delete contents of folder  (overrule 00LOCK ) (e.g., it would delete ../rio/rio_0.5.16) before installing the latter
                #lib_full= paste0(snowball$installation.path[k] ,"/",snowball$pkg_vrs)
                #unlink(snowball$installation.path[k], recursive = TRUE, force = FALSE)
                
              #Install it
                install.packages(url, repos = NULL, lib = snowball$installation.path[k], type = "source", dependencies = FALSE, quiet = quiet.install, INSTALL_opts = '--no-lock')
                
              } #End if source
            
         #4.4 Load 
            .libPaths(c(.libPaths(), snowball$installation.path[k] ))
            loadNamespace(snowball$pkg[k], lib.loc =  snowball$installation.path[k]) 
          
          } #End loop over snowball        

  }#end of function
          
     