#' Get installation time
get.installation.time=function(pkg,vrs)         {
  dfk=subset(cran.times,pkg_vrs == paste0(pkg,"_",vrs))       #subset of package
  if (nrow(dfk)==1) return(dfk$installation.time)             #lookup installation times
  if (nrow(dfk)!=1) return(180)                               #if not found, assume 3 minutes
}
