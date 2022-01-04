#Check if local databases need to be updated, which occurs if a date newer than toc, or R version newer than toc
#is used


  update.databases.if.needed <- function(date) {
  
    
  # 1 Format entered date by user
      date <- as.DateYMD(date)

  #2 Stop if date is more recent than 2 days ago
      if (date > Sys.Date() - 2) {
        message2()
        message1(
          "To ensure reproducibility of your script, given timezone differences and \n",
          "delays in updating different CRAN mirrors, don't use a date more \n",
          "recent than two days ago: ", format(Sys.Date() - 2), "."
        )
    	exit("Invalid date.")
      }

  #2 Load databases 
      load.databases()
      
      #These variables now exist:
        #.pkgenv[["cran.times"]]         
        #.pkgenv[["full_toc"]]           
        #.pkgenv[["missing.mran.dates"]] 
      
  #3 Localized name for easier comparisons
      full_toc <- .pkgenv[["full_toc"]]           
      
  # 3 If user wants  a newer date than available, or if their version of R is newer than that in cran.toc.rds - update it.
      full_toc$Published <- as.DateYMD(full_toc$Published)   #Ensure formatting
      max.date           <- max(full_toc$Published)          #Most recent date in full_cran

  # 4 Compare most recent to entered date
    if (max.date < date) {
        message2()
        message1(
          "The date you entered, '", format(date), "' requires updating your local database\n",
          "used by groundhog to find CRAN packages."
          )
    # Update the database and load them
      return(load.databases(update=TRUE))
      }

  # 5 Also update if the  version of R being used is newer than that in cran.toc.rds
  # Get version of R being used
      R.using <- get.rversion()
      tocR <- toc("R")
      # If not in tocR, update
      if (! (R.using %in% tocR$Version)) {
        message2()
        message1(
          "The local databases used by groundhog to find CRAN packages is older\n",
          "than the version of R you are using. Updating the databases now."
        )
  
      return(load.databases(update=TRUE))
    } else {
      return(FALSE)
    }
  }
