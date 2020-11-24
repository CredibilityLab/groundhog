#' Load `cran.toc`
#'
#' Unload a package upon request, unloading all its parents (reverse-dependencies)
#'
#' @param pkg character, name of package to be unoaded
#'
#' @return (invisibly) `TRUE`/`FALSE` depending on the success/failure of this
#'   operation.
#'
#'
# @examples
# \donttest{
# groundhog:::groundhog.unload('rio')
# }

#Function 1 - get parents (reverse-dependency) of a package  in a two-column table with |pkg|dep| as columns
  get.parents <- function(dep12, dep0) {
      dep12[dep12$dep %in% dep0,]
      }
  
  
#Function 2 - Recursively get all parents from a dependency
  get.all.parents <- function(dep12, pkg)
    {
     #Start with desired package parents
      all.parents <- dep12[dep12$dep %in% pkg,]
      new.parents <- all.parents 
      
    #Find all these packages parents
     
     for (k in 1:15000)  #limit to 15000 loops
     {
      new.parents <- get.parents(dep12, all.parents$pkg) 
      if (nrow(new.parents)==0) break
      new.parents=new.parents[!( paste0(new.parents$pkg,":", new.parents$dep) %in% 
                               paste0(all.parents$pkg,":", all.parents$dep)),]
      all.parents=rbind(all.parents,new.parents)
      k=k+1
     }
    return(all.parents)
  }
    

#Function 3 - unloading itself 
    groundhog.unload <- function(pkg)
    {
      
     #Find active packages
      active <- groundhog:::get.active()
  
    #Add pkg_vrs to crantoc
      cran.toc <- .pkgenv[["cran.toc"]]
      cran.toc$pkg_vrs <- paste0(cran.toc$Package,"_",cran.toc$Version)
  
    #Keep to-be-used cran.toc
      cran.toc.active <- cran.toc[cran.toc$pkg_vrs %in% active$pkg_vrs,]
    
    #Start two-column data.frame
      dep12 <- data.frame(pkg=character(),dep=character())
                     
    #populate package-dependency table
      for (k in 1:nrow(cran.toc.active))
        {
        dep <- c(cran.toc.active$Depends[k] , cran.toc.active$Imports[k])
        dep <- unlist(strsplit(dep, ","))  
        dep <- unique(dep)                 
        if (length(dep)>0) dep12 <- rbind(dep12, data.frame(pkg=cran.toc.active$Package[k], dep=dep))
        }
     #Get all parents
        all.parents=get.all.parents(dep12, pkg)
    
        
    #Sort them in an unloadable order
          indep=c()
         while (nrow(all.parents) > 0) {
          k <- k + 1
          indep.rows <- !(all.parents$pkg %in% all.parents$dep) ## Find parents that are not dependencies
      # Add those packages to the list of independencies
          indepk <- unique(as.character(all.parents$pkg[indep.rows]))
          indep <- c(indep, indepk)
      # Drop those rows from both
        all.parents <- all.parents[!indep.rows, ]
      # Safety valve in case loop impossible to end
        if (k == 50000) {
          break
        }
         }
          
      #Add package itself to the list
          indep=c(indep,pkg)
      #Start unloading from the bottom of the table

        for (pkgk in indep) 
        {
        unloadNamespace(pkgk)
        message1('groundhog says: attempting to unload: ',pkgk)
        }

    }
  
    
    #Function 4. Unload all packages, and their parents, if they are of a different version from that in a  snowball
      unload.conflicts <- function (snowball)
      {
       active=get.active()
       pkgs.to.unload=snowball[(snowball$pkg %in% active$pkg ) & (!snowball$pkg_vrs %in% active$pkg_vrs ),]$pkg

      for (pkgk in pkgs.to.unload)
        {
        groundhog.unload(pkgk)
        }  #End of for loop
      }    #End of function
    
    
   