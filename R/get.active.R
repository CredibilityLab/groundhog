# Get active packages
#
# Get currently active (=loaded) packages in the session, with their version
#
# return a `data.frame` with two columns, pkg & pkg_vrs



get.active <- function() {
   
  
    si <-utils::sessionInfo()
    

   
    loaded.list   <- si$loadedOnly # pkgs in name space
    attached.list <- si$otherPkgs  # pkgs in attached
    base.pkg <- si$basePkgs
    active.pkg <- c(names(loaded.list), names(attached.list)) # Get names of active packages
    active.vrs <- c(lapply(loaded.list, function(x) x$Version), lapply(attached.list, function(x) x$Version))
     
  #Get  base packages
     active.base.pkg <- si$basePkgs
     active.base.vrs <- rep(get.rversion(),length(active.base.pkg))

  #Combine
    active.pkg <- c(active.pkg, active.base.pkg)
    active.vrs <- c(active.vrs, active.base.vrs)
    active.pkg_vrs <- paste0(active.pkg, "_", active.vrs) # merge to pkg_vrs

  
  # Drop those in base R
  #active.in.base <- active.pkg %in% base_pkg()
  #active.pkg <- active.pkg[!active.in.base]
  #active.vrs <- active.vrs[!active.in.base]
  #active.pkg_vrs <- active.pkg_vrs[!active.in.base]

  df <- data.frame(active.pkg, active.pkg_vrs, stringsAsFactors = FALSE)
  names(df) <- c("pkg", "pkg_vrs")
  df
}


get.attached <- function()
{
  si <- utils::sessionInfo()
  #Non-base
     attached.list <- c(si$otherPkgs)
     attached.pkg <- names(attached.list)
     attached.vrs <-  lapply(attached.list, function(x) x$Version)

  #Base 
     base.pkg <- si$basePkgs
     base.vrs <- rep(get.rversion(),length(base.pkg))
 
  #Combine
     pkg <- c(attached.pkg, base.pkg)
     vrs <- c(attached.vrs, base.vrs)
     pkg_vrs <- paste0(pkg, "_", vrs) # merge to pkg_vrs

   df <- data.frame(pkg, pkg_vrs, stringsAsFactors = FALSE)
   
   return(df)
}


  get.installed <- function()
  {
  ip <- data.frame(utils::installed.packages(.libPaths()),row.names = NULL)
  df <- data.frame(pkg=ip$Package, pkg_vrs=paste0(ip$Package,"_",ip$Version), stringsAsFactors = FALSE )
  df <- df[!duplicated(df$pkg),]  #Drop duplicate packages so we only consider the 1st one, which is the one that would get loaded
  return(df)
}

