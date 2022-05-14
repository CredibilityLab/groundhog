
    
  
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
        
  #3 Early return if already attached
            if (pkg_vrs %in% attached.pkg_vrs) {
                  message1("The package '", pkg_vrs, "' is already attached.")
                  return('already_attached')  
            }
                
              #Early return so that groundhog.library.single() knows to stop processing this pkg, 
              #but if more where submitted in pkg=c(pkg1,pkg2) it will just move on to the next package
              #instead of ending the entire groundhog.library() call
      
  #4 Another version already attached
        #Add message if dates mismatch across groundhog library calls
          if ((pkg %in% attached.pkg) &  (!pkg_vrs %in% attached.pkg_vrs)) {
            txt <- paste0(
                    "|IMPORTANT\n",
                    "|    Groundhog says: another version of '", pkg,"' is already attached ('", active$pkg_vrs[active$pkg==pkg],"').\n"
                      )
           
           
        #Add message if dates mismatch across groundhog library calls
         if (length(.pkgenv[['hogdays']])>1) {
            txt <- paste0(txt, 
                    "|    In this R session, you have used different dates to load packages.\n",
                    "|    Dates you have used: ",pasteQC(.pkgenv[['hogdays']]), "\n",
                    "|    If possible use the same date for all packages\n"
                  ) #End of paste0
            
            
          }
           
          txt<-paste0(txt,
                      "|    You may restart the R session to unload all packages.\n",
                      "|    (In R Studio CMD/CTRL-SHFT-F10)\n\n",
                      "|    Type 'OK' to confirm you have read this message.")
           
          infinite.prompt(txt,'ok')
          exit()
         }
        
    

        return('')
        
    } #End function