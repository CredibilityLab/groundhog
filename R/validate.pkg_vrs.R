
    
  
  validate.pkg_vrs <- function(pkg, vrs, date, ignore.deps)
  { 
  
  # -1 If base pkg, stop
    if (pkg %in% base_pkg()) return('')
  
  #0 Merge pkg_vrs
    pkg_vrs <- paste0(pkg, "_", vrs)
  
    
  #1 No 'groundhog'
        if ("groundhog" == pkg) {
        txt <- paste0(
                "|IMPORTANT\n",
                "|     groundhog says: You may not use groundhog.library() to load groundhog.\n",
                "|     To load the version of groundhog available on '", date, "', you may run:\n",
                "|     meta.groundhog('" ,  date , "')",
                "|     Type OK to confirm you have read this message."
                 )
        
        answer <- infinite.prompt(txt,'ok')
        exit()
        } #End if groundhog

    
     
  #2 Read Active  
    #2.1 Read active
       active <- get.active()
       
       
    #2.2 Read attached
        attached.list= utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- unlist(lapply(attached.list, function(x) x$Version))
        attached.pkg_vrs <- paste0(attached.pkg , "_" , attached.vrs)
        attached.pkg_vrs <- paste0(utils::sessionInfo()$basePkgs,"_",getRversion()) #add base pkgs
        

  #3 Early return if already attached
            if (pkg_vrs %in% attached.pkg_vrs) {
                  message1("The package '", pkg_vrs, "' is already attached.")
                  return('already_attached')  
            }
                
              #Early return so that groundhog.library.single() knows to stop processing this pkg, 
              #but if more where submitted in pkg=c(pkg1,pkg2) it will just move on to the next package
              #instead of ending the entire groundhog.library() call
      
 
        return('')
        
    } #End function