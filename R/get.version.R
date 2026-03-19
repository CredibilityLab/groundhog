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
    version.k <- max(c(1,which(dfk$Published < date)))
          # Position of last package available before the date
         
          # on 2024 09 29 added max(c(1,)) so that if one choose the date when pkg was released
          # it is based on the 1st day availalble as there is a hard <
          # Could use <= but it could have break existing groundhog code which loaded oother version of
          # pkgs, so this was more conservative.
    
    current.version <- dfk$Version[version.k]
  
   
  #4 Output version
  if (patch == "current") {
    return(current.version)
  } else if (patch == "max") {
    nopatch <- gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", current.version)
    return(dfk$Version[max(which(gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", dfk$Version)==nopatch))])
  }
}

# Batch version lookup: one cran.toc subset for all pkgs (used in get.snowball step 4)
# @param pkgs character vector of package names
# @param date date (yyyy-mm-dd) to resolve versions for
# @return character vector of versions, same order and length as pkgs
get_version_many <- function(pkgs, date) {
  if (length(pkgs) == 0L) return(character(0L))

  # Ensure cran.toc loaded and up to date for this date
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }
  update_cran.toc_if.needed(date = date)
  cran.toc <- .pkgenv[["cran.toc"]]
  cran.toc$Published <- as.DateYMD(cran.toc$Published)
  last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
  if (date > last.toc.date) {
    exit(
      "The index of CRAN packages ends on ", last.toc.date,
      " which is before the date you entered: ", format(date, "%Y-%m-%d"), ".\n"
    )
  }

  # Base packages: use installed version
  base.pkgs <- base_pkg()
  is.base <- pkgs %in% base.pkgs
  if (any(is.base)) {
    ip.base <- data.frame(utils::installed.packages(priority = "base"), stringsAsFactors = FALSE)
  }

  # Non-base: one subset of cran.toc for all requested packages
  non.base.pkgs <- unique(pkgs[!is.base])
  if (length(non.base.pkgs) > 0L) {
    sub <- cran.toc[cran.toc$Package %in% non.base.pkgs, c("Package", "Version", "Published")]
    sub <- sub[order(sub$Package, sub$Published), ]
  } else {
    sub <- cran.toc[0L, c("Package", "Version", "Published")]
  }

  out <- character(length(pkgs))
  for (k in seq_along(pkgs)) {
    pkg <- pkgs[k]
    if (is.base[k]) {
      ip.base.pkg <- ip.base[ip.base$Package == pkg, ]
      out[k] <- max(ip.base.pkg$Version)
      next
    }
    dfk <- sub[sub$Package == pkg, c("Version", "Published")]
    if (nrow(dfk) == 0L) {
      msg <- paste0(
        "According to our records, the package '", pkg, "' was not yet available on CRAN on '", format(date, "%Y-%m-%d"), "'"
      )
      gstop(msg)
    }
    if (dfk$Published[1L] > date) {
      msg <- paste0(
        "According to our records, the package '", pkg, "' was not yet available on CRAN on '", format(date, "%Y-%m-%d"), "'"
      )
      gstop(msg)
    }
    version.k <- max(1L, which(dfk$Published < date))
    out[k] <- as.character(dfk$Version[version.k])
  }
  out
}
