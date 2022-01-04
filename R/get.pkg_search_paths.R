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

  
  #Assess origin of pkg
    remote_id <- get.remote_id(pkg)
    
  #Default value
    pkg_search_paths <- NULL
    
  #Cran packages 
    if (remote_id=='cran') 
      {
      pkg_search_paths <- paste0(get.groundhog.folder(), "/R-", rv, "/", pkg, "_", vrs)
      }

    
    ###>>>> HERE, NEED T FIND WAY TO PASS ON SHA, TO THE SEARCH FOR IT IS SAVED WITH IT
  #If remote
    if (remote_id %in% c('gitub','gitlab')) {
      pkg_search_paths <- paste0(get.groundhog.folder(), "/R-", rv, "/_" , remote_id, 
      
    }
    
  # Ensure directories exist
  return(pkg_search_paths)
}
