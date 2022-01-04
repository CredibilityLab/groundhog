# Get package version from date
# Example
# groundhog:::get.version("magrittr", "2018-02-12")

get.version <- function(pkg, date, patch = c("current")) {


    #if it is a base package, return the installed version
    if (pkg %in% base_pkg()) {
        ip.base     <- data.frame(utils::installed.packages(priority="base"), stringsAsFactors = FALSE)    
        ip.base.pkg <- ip.base[ip.base$Package == pkg,]
          
        #if a user has a misconfigured copy of R, they may have two version of the same base package, take the highest of them 
          #return(ip.base.pkg$Version) #up to 2021 - 10 - 30
        
        return(max(ip.base.pkg$Version))
        }
    
  
  # 1. Load toc for pkg and full_toc
    dfk <- toc(pkg)
    full_toc <- get.full_toc()  

        #available from: get.full_toc.R 
        # This function returns the cran.toc.rds loaded into the environment 
        # and when groundhog.library() calls on a remote pkg, it has those packages there
        # (within the groundhog.library() call, returning to original .rds when task is completed
    
  # 2 Check if date request comes after first date for that package
    if (dfk$Published[1] > date) {
      message2()
      message1("According to our records, the package '", pkg, "' was not yet available on CRAN on '", date, "'")
      exit()
     }
    

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
