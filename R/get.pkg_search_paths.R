# Get package search path
# Determine paths to search for an already installed package (versions of same
# minor, in reverse chronological order).
#
#example
#groundhog:::get.pkg_search_paths("magrittr", "1.0.1")
#
get.pkg_search_paths <- function(pkg, vrs) {

  #If base R, default library
    if (pkg %in% base_pkg()) {
      
      #Get libpath
        libp <- .libPaths()
        
      #Get last one
        default_lib <- libp[length(libp)]
        
      #Add package to name
        full_path <- paste0(default_lib , "/", pkg)
      #End
        return(full_path)
        }
  
  rv <- as.character(getRversion())

  # Get rid of patch version number
  rv <- gsub("\\.\\d+(-w)?$", "", rv)

  # paths
  pkg_search_paths <- paste0(get.groundhog.folder(), "/R-", rv, "/", pkg, "_", vrs)

  # Ensure directories exist
  return(pkg_search_paths)
}
