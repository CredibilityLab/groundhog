

    validate_R <-function(date, tolerate.R.version)
      {

      
    #1 Check R version
      rv <- r.version.check(date) # Get version of r being used and needed
    
    #2 If using version of R that is more advanced than most advanced in cran.toc.rds
      R.toc <- toc("R")
      if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
          {
          #two days ago
            two.days.ago <- Sys.Date()-2
            
          #update temporarily cran.toc.if needed
            max.date <- (max(.pkgenv[["cran.toc"]]$Published))
            if (max.date < two.days.ago) load.cran.toc(update.toc = TRUE)
          
          #get R version again  
            rv <- r.version.check(date)
            
          #If version being used is newer than newest
            if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
              {
              
            #Show message that R-dev is being temporarily added
              msg <- paste0("The version of R you are using, 'R-" , rv$r.using.full, "', is not included ",
                            "in groundhog's database of R releases. Assuming this is because you are using ",
                            "an R-dev version for testing purposes, the version 'R-" , rv$r.using.full, "'",
                            "will be temporarily added to your local database as if it was released 2 days ago. ",
                            "This  modification will be undone when the R session is restarted.")
              
                message(msg)
              
                              
           #Add r-dev to cran.toc
              row <- data.frame(Package="R",Version=rv$r.using.full,Published = two.days.ago, Imports="",Depends="",Suggests="",LinkingTo="", stringsAsFactors=FALSE)
              .pkgenv[["cran.toc"]] <- rbind(.pkgenv[["cran.toc"]], row)
               
              } #End if version not in cran.toc even after update
            }   #End if version not in cran.toc before update
    
      
    #3 Validate tolerate.R.version
      if (!tolerate.R.version %in% c("",rv$r.using.full)) {
        msg<- paste0("Groundhog says: the version of R you are using '",    rv$r.using.full,  "' does ",
                     "not match the version entered in the tolerate.R.version argument, '",tolerate.R.version,"'. ",
                     "You must either drop that argument or replace it with the version of R ",
                     "you are currently using.  Please type 'OK' to confirm you read this message.")
       gstop(msg)  #utils #51
      }
      
    #3 If R needed does not match R using, and was not authorized

      if (package_version(rv$r.using.majmin) != package_version(rv$r.need.majmin) &&  (!rv$r.using.full == tolerate.R.version) )
      {
            
      #3.1 Find date range for R-using
        #Subset of R.toc with current version   
          R.toc.current <-subset(R.toc, as.numeric(regexpr(rv$r.using.majmin, R.toc$Version) )==1)

        #To avoid breaking errors, if the version were not to be found, simply give a warnin
          if (nrow(R.toc.current)==0)
          {
            message('Warning: was not able to find dates for current version of R being used.')
            return(FALSE)
          }
          
        #First and last date in that subset
            min.date <- min(R.toc.current$Published)    
            max.date <- max(R.toc.current$Published)
            
        #If using the latest version of R, make max.date two days ago
            if (max.date==max(R.toc$Published)) 
              {
              max.date <- Sys.Date() - 2
              } else {
        #Else, use the date of the next release (found as the lowest published date coming after the set for this version)
               max.date <- min(R.toc$Published[R.toc$Published > max.date])-1
              }
            
        
            msg <- paste0(
                "Groundhog says: you are using R-", rv$r.using.full, ", but the version of R ",
                "current for the entered date, '", date, "', is R-", rv$r.need.majmin, ".x. ",
                "It is recommended that you either keep this date and switch to that version ",
                "of R, or you keep the version of R you are using but switch the date to between ",
                "'" , min.date , "' and '" , max.date , "'. \n \n ",
                "You may bypass this R-version check by adding: `tolerate.R.version='",rv$r.using.full,"'`",
                "as an option in your groundhog.library() call.")
				
			gstop(msg) #Util 51
      
      } else {
       return(invisible(TRUE))
      }
    } #End function