# Get installation time for a given `pkg` and `vrs`. This is estimated from
# CRAN data about installation time on their build servers.
#
# Example:
# groundhog:::get.installation.time("magrittr", "1.5")
# 

get.installation.time <- function(pkg, vrs) {
  load.databases()
  #Ensures the .pkgenv[[]] for the three databases are loaded
  
  cran.times <- .pkgenv[["cran.times"]]

  dfk <- cran.times[cran.times$pkg_vrs == paste0(pkg, "_", vrs), ] # subset of package
  if (nrow(dfk) == 1) {
    return(dfk$installation.time) # lookup installation times
  } else {
    return(180)
  } # if not found, assume 3 minutes
}
