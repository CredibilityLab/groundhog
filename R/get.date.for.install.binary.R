# Get date of binary package-version availability for current R version, on  MRAN


get.date.for.install.binary <- function(pkg_vrs , date) {
  
  
  #Format date for not found and entered date
    binary.date <- as.DateYMD("1970-01-01")
    date <- as.DateYMD(date)
  
  
  #0 Return 1970-01-01  if it is a base package, 
    pkg <- get.pkg(pkg_vrs)
    if (pkg %in% base_pkg()) 
      {
      return(binary.date)
      }
      
 
  
  #0.5 R being used
    r.using.full <- get.rversion() # Get current
    r.using.major <- R.version$major
    r.using.minor <- strsplit(R.version$minor, "\\.")[[1]][1]

  #1 Find R1 and R2 (first and last dates with the current version of R (ignoring patch, R-x.z.ignored)
  
    #1.1 Get all R releases
      R.toc <- toc("R") # Get R toc

    #1.2  Find all R versions with same major and minor versions as currently using
      R_vrs <- grep(paste0("^", r.using.major, ".", r.using.minor), R.toc$Version, value = TRUE)

    #1.3 Get the first and last one
      R1=R_vrs[1]
      R2=R_vrs[length(R_vrs)]
      
    #1.4 Get those dates
      R.k1=match(R1,R.toc$Version)  #kth observation with 1st date
      R.k2=match(R2,R.toc$Version)  #kth with last
        
      #First R date based on match of minor  
        date.R1=R.toc[R.k1,]$Published
        
      #Second is either the next version of R, or the current day
        
        #If this is NOT the most recent R, look at the next one
        if (R.k2<nrow(R.toc)) 
            {
            date.R2=R.toc[R.k2 + 1,]$Published  
        #If it is the most recent, look at today
            } else {
            date.R2=Sys.Date() -2  
            }
  #2 Find pkg1 and pkg2 (first and last dates with the desired package version)
    #2.1 Get all package releases
      pkg=get.pkg(pkg_vrs)
      pkg.toc <- toc(pkg) 

    #2.2 Match the date of desired, and grab that and next release as date.pkg1, date.pkg2
      vrs=get.vrs(pkg_vrs)
      date.pkg1 <- pkg.toc[match(vrs,pkg.toc$Version),]$Published    #date when desired version was released
      date.pkg2 <- pkg.toc[match(vrs,pkg.toc$Version)+1,]$Published  #date when next version was released
      
      #If no next release, use highest date in cran.toc.rds
      if (is.na(date.pkg2)) {
          cran.toc <- .pkgenv[["cran.toc"]]
          date.pkg2 <- max(cran.toc$Published)   
          }
      
    #3 Find range of values when package and R version match
      #3.1 Start period
        D1 <- max(date.R1, date.pkg1)
        D2 <- min(date.R2, date.pkg2)
      
      
      #3.2 If there is no overlap, ends before it starts, January 1
        if (D2<=D1) return(as.DateYMD("1970-01-01"))
   
      #3.3 Available dates
          available.dates <- as.DateYMD(D1:D2)
      
      #3.4 Drop MRAN missing dates
          missing.mran.dates <- .pkgenv[["missing.mran.dates"]] 
          available.dates <- available.dates[!available.dates %in% missing.mran.dates]
      
      #3.4.5 LOSING MRAN 
          #Drop dates after Jan 31st, 2023 they are all missing from MRAN
           available.dates <- available.dates[available.dates<as.DateYMD("2023-01-31")]
          
      #3.5 If no dates left, return 1970
          if (length(available.dates)==0)  return(as.DateYMD("1970-01-01"))
          
      
      #3.6 if an available date is 7 days prior to d1 optimize by choosing smallest gap
          if (date - D1 >=7)
            {
            date.gap <- abs(available.dates - as.Date(date))
            date.min.gap <- available.dates[which.min(date.gap)]
            return(date.min.gap)
          }
      #3.7 If the date is less than 7 days from d1, choose the first date after the 7th days, or the highest available
        if (date - D1 <7) {
          
           #How many dates are available a week after released
              available.dates.7.days <- available.dates[available.dates > D1+7]
              
          #If at least 1, choose the first
           if (length(available.dates.7.days)>0) return(min(available.dates.7.days))
              
          #If none, choose the last available date
           if (length(available.dates.7.days)==0) return(max(available.dates))
          }

      
          
  #4 If we have not yet returned, give 1970 date
        return(as.DateYMD("1970-01-01"))
  }
