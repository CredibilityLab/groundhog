#This function installs all binaries passed on in a a snowball, if something is not a binary it will
#be dropped



################################################################################

 install.binaries <- function(snowball, install.sequentially=FALSE)
   {
      
      #1  Directory for downloaded zips
          time0 <- as.numeric(Sys.time())
          temp_path <- paste0(get.groundhog.folder() ,"/temp_",time0)
          dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
    
    
      #1.5 Drop non-binaries
          snowball <- snowball[snowball$from %in% c("CRAN","GRAN"),]
      
                        
      #2 Make  URLs for downlaoding and file-paths for saving
        #2.0 Common beginning of URL
          #CRAN      
            repos     <- as.character(getOption("repos"))
            url.cran  <- contrib.url(repos,type='binary')
            
          #GRAN
            os  <- get.os()
            r.version <- get.r.majmin()
            url.wasabi <- paste0("http://gran.groundhogr.com/", os , "/", r.version, "/")
    
        #2.1 File extension
            if (os=='windows') ext <- 'zip'
            if (os!='windows') ext <- 'tgz'
        
        #2.2 Finalize URL with each file name
            url.files <- ifelse(snowball$from=='CRAN',
                                  paste0(url.cran,                       "/", snowball$pkg_vrs , "." , ext),
                                  paste0(url.wasabi, snowball$GRAN.date, "/", snowball$pkg_vrs , "." , ext))
            
        #2.3 Local name for saving
            zip.files <-file.path(temp_path,basename(url.files))
            
            
        #3 DOWNLOAD
          #3.1 Message
            n.cran <- sum(snowball$from=="CRAN")
            n.gran <- sum(snowball$from=="GRAN")

            if (n.cran>0 & n.gran>0)  message1("Will now download ",n.cran, " packages from CRAN, and ",n.gran," from GRAN")
            if (n.cran>0 & n.gran==0) message1("Will now download ",n.cran, " packages from CRAN")
            if (n.cran==0 & n.gran>0) message1("Will now download ",n.cran, " packages from GRAN")

            
          #3.2 Download 
               for (k in 1:length(url.files))
                {
                download.file(url.files[k], zip.files[k])
                } #End loop downloading
            
            

        #4 Install them
            
            #4.1 Read downloaded zip files
              all.zip <- list.files(temp_path,full.names = TRUE)
              n.zip <- length(all.zip)
              message1("Will now install ",n.zip, " packages.")
            
            #4.2 Unzip all files found
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
                
                  message1('Installing ',basename(zk))
                
                #Unzip  
                  if (ext=="zip") utils::unzip(zk, exdir=outfile)
                  if (ext!="zip") utils::untar(zk, exdir=outfile)        
             
              }

          
      #5 Verify installation
          ip <- data.frame(installed.packages(snowball$installation.path))      
          ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
          
        #Add success column to snowball
          snowball$success <- snowball$pkg_vrs %in% ip$pkg_vrs
  
          
      #6 delete temp folder
          unlink(temp_path, recursive = TRUE)
          
      #7 Output
          return(snowball)
          #note: if a non-binary was included in snowball, it will not appear here

    }

   
 