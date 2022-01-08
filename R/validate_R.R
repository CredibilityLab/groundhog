

    validate_R <-function(date, tolerate.R.version)
      {

    #1 Check R version
      rv <- r.version.check(date) # Get version of r being used and needed
    
    #2 If using version of R too advance for groundhog
      R.toc <- toc("R")
      if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
          {
          #two days ago
            two.days.ago <- Sys.Date()-2
          #update temporarily cran.toc.if needed
            max.date <- (max(.pkgenv[["cran.toc"]]$Published))
            if (max.date < two.days.ago) load.cran.toc(update.toc = TRUE)
            rv <- r.version.check(date) 
            if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
              {
              message("groundhog says:")
              message("The version of R you are using, 'R-" , rv$r.using.full, "' is not in the groundhog database.")
              message("You can temporarily add it. This is a work-around intended for testing groundhog with R versions\n",
                      "that have not yet been released. You should not consider this script reproducible.\n",
                      "To write reproducible R code please use an already released version of R instead. \n\n",
                      "Type OK to temporarily add ", rv$r.using.full , " to your local groundhog database\n",
                      " (it will be added as if it were released 2 days ago, and when you restart the R session it will be removed).")
              text <- readline("Enter OK to add, anything else to stop >")
              
              #If say OK, add row to database temporarily
                if (tolower(text)=="ok")
                {
                  row <- data.frame(Package="R",Version=rv$r.using.full,Published = two.days.ago, Imports="",Depends="",Suggests="",LinkingTo="")
                  .pkgenv[["cran.toc"]] <- rbind(.pkgenv[["cran.toc"]], row)
                } else {
              #Else, end
                  message("Your typed '" , text, "!=OK, groundhog.library() request will terminate here.")
                  exit()
                 } #end else
              } #End if version not in cran.toc even after update
            }   #End if version not in cran.toc before update
    
    #2.1 Is date for a later major R? STOP

    if ((package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) &  rv$r.using.full!=tolerate.R.version) 
      {
      message2()
      message(
        "You are using R-", rv$r.using.full, " and the current R version for the date you entered:",
        "'", date, "' was R-", rv$r.need.majmin, ".\n",
        "It is recommended that you use the matching version of R, but you may bypass this error message\n",
        "by adding: `tolerate.R.version='",rv$r.using.full,"'` as an option to your groundhog.library() call.",
        "\n\n   ----------------- Package(s) NOT LOADED ----------------"
      )
      exit()
    }
    
        
    #2.1.5 throw warning if using R that is too old, but explicitly allowing via command 
    if ((package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) & 
        (rv$r.using.majmin==tolerate.R.version | rv$r.using.full==tolerate.R.version)) {
          message2()
          message(
           "You are using R-", rv$r.using.full, " and the current R version for the data you entered:",
           "'", date, "' was R-", rv$r.need.majmin, ".\n",
           "Usually this results in an error and groundhog stops processing the request, but\n",
           "you are receiving only a warning because you explicitly allowed version 'R-", tolerate.R.version, "'.\n"
          )
            
            
         }

        
    
    #2.2 Is date for a previous major R? Warn
    
      if (package_version(rv$r.using.majmin) > package_version(rv$r.need.majmin)) {
          
         #Path to text file to keep track if warning already shown
            cookie_path <- paste0(get.groundhog.folder(),"/warning_r_mismatch.txt")
        
          #Path to full text of code submitted that we amay want to execute (code interrupted by prompt)
            all.text_path <-paste0(get.groundhog.folder(),"/all_text_mismatch_r.txt")
        
            
          #How long since last warning?
            since_warning <- 31  #assume 30 minutes , i.e., show warnings
            if (file.exists(cookie_path)) since_warning <- difftime(Sys.time(),file.info(cookie_path)$mtime,units='mins')
			      
          #If >24 show warnings
          if (since_warning>30)
          {
          #Update cookie to indicate warning has been shown now
            unlink(cookie_path)

          #Show warning

          message2()
          message1(
            "You are using R-", rv$r.using.full, ", but on ","'", date, "' the current version was R-", rv$r.need.majmin, ".\n",
            "Old packages can take longer to install, and old code in general and old packages\n",
            "in particular can give different results, or not run at all, in newer R versions.\n",
            "You may want to either change the date you entered or the version of R you use.\n",
            " - To change the date, choose something after '",get.r.majmin.release(),"'\n",
            " - For instructions to run older versions of R (e.g. R-",rv$r.need.full, "), see http://groundhogr.com/many\n\n")
          
          #While loop for readline to avoid submitted code to be interpreted as the answer
            len.answer <- 0
            text <-''
            j <- 1 #counter of times message is shown
            while (strip.prompt(text)!="x" & strip.prompt(text)!="ok")  
            {
              prompt.text <- paste0("Type 'OK' to ignore this warning about the date entered, or type 'X' to stop >")
              text <- readline(prompt.text)
              if (strip.prompt(text) !="ok" & strip.prompt(text) != "x") {
                    
                  message(j , ") You answered: '", text , "'") 
                  message("   To ensure you are actively answering, only 'OK' and 'X' are accepted as responses\n")
                  j<-j+1 #Add to counter of msgs rejected
                } #end if
            } #End while
          
            
          #If they press stop, don't load/install package
            if (strip.prompt(text)=="x") {
              message("You typed 'X'. Stopped.")
              exit()
              } else {
              message1("OK. We will continue. This warning will not be shown again within 30 minutes.\n")
              write("1",cookie_path)
            
              } #End else
          } #Showed warnings
        
          
      }
      
    }#End function