#' Show toc table with multiple packages
cross.toc=function(pkgs,date1="1970-1-1",date2=Sys.Date())
{

  if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)
  n=length(pkgs)
  toc.all=toc(pkgs[1])
  toc.all$Package=pkgs[1]
  for (k in 2:n)
  {
    tock=toc(pkgs[k])
    tock$Package=pkgs[k]
    toc.all=rbind(toc.all,tock )
  }

  #Sort
  toc.all=toc.all[order(toc.all$Published),]
  #date subset
  return(subset(toc.all,Published>date1 & Published<date2))

}
