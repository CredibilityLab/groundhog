
    
  
  validate.pkg_vrs <- function(pkg, vrs, date, ignore.deps)
  { 
  
  #1 If base pkg, stop
    if (pkg %in% base_pkg()) return('')
  
  #2 Merge pkg_vrs
     pkg_vrs <- paste0(pkg, "_", vrs)
  
    
  #3 No 'groundhog'
        if ("groundhog" == pkg) {
        txt <- paste0(
                "|IMPORTANT\n",
                "|     groundhog says: You may not use groundhog.library() to load groundhog.\n",
                "|     To load the version of groundhog available on '", date, "', you may run:\n",
                "|     meta.groundhog('" ,  date , "')",
                 )
        
        message(txt)
        exit()
        } #End if groundhog

    
     
  #4 Read Active  
    #4.1 Read active
       active <- get.active()
       
       
    #4.2 Read attached
        attached.list= utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- unlist(lapply(attached.list, function(x) x$Version))
        attached.pkg_vrs <- paste0(attached.pkg , "_" , attached.vrs)
        attached.base.pkg_vrs <- paste0(utils::sessionInfo()$basePkgs,"_",getRversion()) #add base pkgs
        attached.pkg_vrs <- c(attached.pkg_vrs,attached.base.pkg_vrs)

  #5 Early return if already attached
        if (pkg_vrs %in% attached.pkg_vrs) {
          message1("The package '", pkg_vrs, "' is already attached.")
          return('already_attached')  
        }
  
  #6 End otherwise              
        return(invisible(''))
        
    } #End function