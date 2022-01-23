


  verify.snowball.loaded<-function(snowball, ignore.deps)
  {
    
    #1 Extract pkg vrs of target pkg from snowball
      n <- nrow(snowball)
      pkg     <- snowball$pkg[n]  
      pkg_vrs <- snowball$pkg_vrs[n]  
    
      
    #2 Did target package attach 
        
        #2.0 Get Attached packages info
          attached.list <- utils::sessionInfo()$otherPkgs # pkgs in attached
          attached.pkg  <-names(attached.list)
          attached.vrs <- lapply(attached.list, function(x) x$Version)
          attached.pkg_vrs <- paste0(attached.pkg, "_", attached.vrs)  
         
            
        #2.1 Pkg not attached
          if (!pkg %in% attached.pkg) {
              message("groundhog says: FAILED to load '", pkg_vrs,"'")
              exit()
              }
    
        #2.2 Unexpected version attached
                if ((pkg %in% attached.pkg) & (!pkg_vrs %in% attached.pkg_vrs))
                      {
                      message("groundhog says: WARNING, loaded unexpected version of '", pkg, "'\n",
                             "expected: '", pkg_vrs, "'\n",
                             "loaded  : '", attached.pkg_vrs[attached.pkg == pkg], "'\n")
                      exit()
                      }
       
        #2.3 Successfully attached pkg_vrs
            if (pkg_vrs %in% attached.pkg_vrs)
              {
              message1("Succesfully attached '", pkg_vrs,"'")
              } 
      

    #3 Check if entire snowball is available 
     
      #3.1 active
         active <- get.active()                          
    
      #3.2 Counts of mismatches
          snowball.match <- snowball$pkg_vrs %in% active$pkg_vrs  | snowball$pkg %in% base_pkg() | snowball$pkg %in% ignore.deps
          
            #There is an or stateent so taht base packages are not checked against available ones becuase
            #Those have their own library structure
          
          n.mismatch <- sum (!snowball.match) 
          n <- nrow(snowball)
         
       

      #3.5 Warning if something is not available  is available
         
         if (n.mismatch>0) {
          message("WARNING\n",
                  "The package '",pkg_vrs,"' depends on ", n, " other pacakges.\n",
                  "Groundhog should have obtained the versions of those packages as\n",
                  "available on the entered date. However, the final check identified that ", n.mismatch, "\n",
                  "of them were obtained for a version which does not match.\n",
                  "Please check out 'https://groundhogr.com/troubleshooting' to see if the\n",
                  "source of this problem is known.")
           
          #Use utils.R function to get formatted and sorted list of needed vs found
           #mismatched_versions_report <- get.mismatched_versions_report(found.pkg_vrs = available.pkg_vrs, need.pkg_vrs = snowball$pkg_vrs)
           #message1(mismatched_versions_report) 
           return(FALSE)
           }

     

            
  #12 Message on base pkg
          
          if (pkg %in% base_pkg()) {
            message("Note: the package '", pkg, "' is part of base R.\n",
              "The version included in base R is always loaded, regardless of the date entered.")
          }
							
  
  #13 Snowball was validly loaded if n.mismatch == 0
          validated <- (n.mismatch == 0)
          return(validated)
							
							
  } #End of verification