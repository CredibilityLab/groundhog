
  verify.snowball.loaded<-function(snowball, ignore.deps)
  {
    
    #0 Get active
      active <- get.active()
    
  
    
    #1 Extract pkg vrs of target pkg from snowball
      n <- nrow(snowball)
      pkg     <- snowball$pkg[n]  
      pkg_vrs <- snowball$pkg_vrs[n]  
    
      
    #2 Did target package attach 
        
        #2.0 Get Attached packages info
          attached<-get.attached() #in get.active.R
         
            
        #2.1 Pkg not attached
          if (!pkg %in% attached$pkg) {
              msg = paste0("groundhog says: FAILED to load '", pkg_vrs,"'")
              gstop(msg)
              }
    
        #2.2 Unexpected version attached
                if ((pkg %in% attached$pkg) & (!pkg_vrs %in% attached$pkg_vrs))
                      {
                      msg <- paste0("groundhog says: WARNING, loaded unexpected version of '", pkg, "'\n",
                             "expected: '", pkg_vrs, "'\n",
                             "loaded  : '", attached$pkg_vrs[attached$pkg == pkg], "'\n")
                      gstop(msg)
                      }
       
        #2.3 Successfully attached pkg_vrs (if it is attached now, and was not attached before
          #Already attached
          if (pkg_vrs %in% attached$pkg_vrs & pkg_vrs %in% .pkgenv[['attached']]$pkg_vrs) #See groundhog.library #5
              {
              message1("Previously attached  '", pkg_vrs,"'")
           } 
          #Newly attached
            if (pkg_vrs %in% attached$pkg_vrs & !pkg_vrs %in% .pkgenv[['attached']]$pkg_vrs) #See groundhog.library #5
              {
              message1("Successfully attached '", pkg_vrs,"'")
              } 

    #3 Check if entire snowball is available 
     
      #3.1 active
         active <- get.active()                          
         ip <- get.installed()  #get.active.R (if multiple versions available, it returns the first one in .libPaths())
         
      #3.2 Counts of mismatches
          snowball.match <- snowball$pkg_vrs %in% ip$pkg_vrs  | snowball$pkg %in% base_pkg() | snowball$pkg %in% ignore.deps
          
            #There is an or statement so that base packages are not checked against available ones because
            #those have their own library structure
          
          n.mismatch <- sum (!snowball.match) 
          n <- nrow(snowball)
         
       

      #3.5 Warning if something is not available  is available
         
         if (n.mismatch>0) {
          msg=paste0("The package '",pkg_vrs,"' depends on ", n, " other packages.",
                  "Groundhog should have obtained the versions of those packages as ",
                  "available on the entered date. However, the final check identified that ", n.mismatch, 
                  " of them were obtained for a version which does not match.",
                  "Please check out 'https://groundhogr.com/troubleshooting' to see if the",
                  "source of this problem is known.")
                  # Among the mismatching packages, the requested ones are ",
                  # pasteQC(snowball$pkg_vrs[!snowball.match])," the obtained packages are ",
                  # pasteQC(ip$pkg_vrs[ip$pkg %in% snowball$pkg[!snowball.match]])
                  #)
           
          #Use utils.R function to get formatted and sorted list of needed vs found
           
           mismatched_versions_report <- get.mismatched_versions_report(found.pkg_vrs = ip$pkg_vrs[ip$pkg %in% snowball$pkg[!snowball.match]],
                                                                        need.pkg_vrs  = snowball$pkg_vrs[!snowball.match])
           message1(mismatched_versions_report) 
           return(FALSE)
           }

     

            
  #12 Message on base pkg
          
          if (pkg %in% base_pkg()) {
               message1("Note: '", pkg, "' is included with R-" , getRversion() , ".")
               message1("That version is loaded regardless of date entered.")
          }
							
  
  #13 Snowball was validly loaded if n.mismatch == 0
          validated <- (n.mismatch == 0)
          return(validated)
							
							
  } #End of verification