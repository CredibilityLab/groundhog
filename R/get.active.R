#' Get Active packages as data.frame()
#'
#' @return  $pkg has the pkg name, $pkg_vrs the pkg_vrs
#'
get.active=function()
{
  loaded.list  =utils::sessionInfo()$loadedOnly                   #pkgs in name space
  attached.list=utils::sessionInfo()$otherPkgs                    #pkgs in attached
  active.pkg=c(names(loaded.list), names(attached.list))          #Get names of active packages
  active.vrs=c(lapply(loaded.list, function(x) x$Version), lapply(attached.list, function(x) x$Version))
  active.pkg_vrs=paste0(active.pkg,"_",active.vrs)                 #merge to pkg_vrs

  #Drop those in base R
  active.in.base=active.pkg %in% dep.base
  active.pkg=active.pkg[!active.in.base]
  active.vrs=active.vrs[!active.in.base]
  active.pkg_vrs=active.pkg_vrs[!active.in.base]

  df=data.frame(active.pkg,active.pkg_vrs)
  names(df)=c("pkg","pkg_vrs")
  df
}
