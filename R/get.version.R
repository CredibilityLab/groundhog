# Get package version from date
# Example
# groundhog:::get.version("magrittr", "2018-02-12")

get.version <- function(pkg, date, patch = c("current")) {

  #if it is a base package, return the installed version
    if (pkg %in% base_pkg()) {
        ip.base     <- data.frame(utils::installed.packages(priority="base"), stringsAsFactors = FALSE)    
        ip.base.pkg <- ip.base[ip.base$Package == pkg,]
          
        #if a user has a misconfigured copy of R, they may have two verson of the same base package, take the highest of them 
          #return(ip.base.pkg$Version) #up to 2021 - 10 - 30
        
        return(max(ip.base.pkg$Version))
        }
    
  
  # 1. Get toc
    dfk <- toc(pkg)
    cran.toc <- .pkgenv[["cran.toc"]]

  # 2.2 Check if date request comes after first date for that package
    if (dfk$Published[1] > date) {
      msg = paste0("According to our records, the package '", pkg, "' was not yet available on CRAN on '", date, "'")
      gstop(msg)
    }
    
  # 2.3 Update cran.toc if  requested date comes before most up to date date
      #Get most recent package date
        last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
      
      #If requested date is after update, and check again
      if (date > last.toc.date) {
    	    update_cran.toc_if.needed(date)
          cran.toc <- .pkgenv[["cran.toc"]]
          last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
          if (date > last.toc.date) {
            exit(
                "The index of CRAN packages ends on ", last.toc.date,
                " which is before the date you entered: ", date, ".\n"
                )
            
              } #End second 'if' current date>cran
          } #End first 'if' 

  
  # 3 Get version
    version.k <- max(which(dfk$Published < date)) # Position of last package available before the date
    current.version <- dfk$Version[version.k]
  
   
  #4 Output version
  if (patch == "current") {
    return(current.version)
  } else if (patch == "max") {
    nopatch <- gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", current.version)
    return(dfk$Version[max(which(gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", dfk$Version)==nopatch))])
  }
}
