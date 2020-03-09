#2.11 FIND BINARY DATE FOR MRAN, given R version being used
get.R.pkg.date=function(pkg_vrs,R_vrs)
{
  #1. Get pkg from pkg_vrs
  pkg=get.pkg(pkg_vrs)
  vrs=get.vrs(pkg_vrs)

  #2. cross.toc with R - all available pkg versions and R
  cross1=cross.toc(c(pkg,"R"))

  #3, Which row have pkg_vrs vs R_vrs
  k.pkg=match(pkg_vrs,paste0(cross1$Package,"_",cross1$Version))
  k.R=match(paste0("R_",R_vrs),paste0(cross1$Package,"_",cross1$Version))

  #4. Both ks in vector
  ks=c(k.pkg, k.R)

  #5. From one to the other (cross2: subset from cross1 where  pkg_vrs to R_vrs or vice versa)
  cross2=cross1[min(ks):max(ks),]

  #6. If the package came first:
  if (k.pkg<k.R)
  {
    #6.1. If there is another version of the package in the subset, it means it changed before arriving at the desired R, so return ""
    if (sum(cross2$Package==pkg)>1) return("1970-01-01")
    #6.2 If there is only one package in the set, then starting with last row, the desired package is available for that R, take midpoint till next
    if (sum(cross2$Package==pkg)==1) {
      start.date=cross1[k.R,  ]$Published  #start.date: start of period when pkg_vrs binary was avilable for this R-version

      #6.3 If not using the most recent R, the midpoint is towardsthe next one, if most recent, halfway to today
      if (k.R<nrow(cross1))  end.date=as.DateYMD(cross1[k.R+1,]$Published)-2  #If already replaced, when it was replaced, minus 2 days for caution
      if (k.R==nrow(cross1)) end.date=as.DateYMD(Sys.Date())-2    #If not yet replaced, still current with today's MRAN, but use minus two days for caution

      #6.4  If end.date not yet in toc, update toc
      if (max(cran.toc$Published)<end.date) load.cran.toc(T)
    }#ENd 6.2 --  if date will be found
  }#End if package came first

  #7. If  R came first:
  if (k.pkg>k.R)
  {
    #7.1. If there is another version of R, different minor, it changed
    if (sum(cross2$Package=="R")>1) return("1970-01-01")
    #7.2 If there is only one version of R in the set,
    if (sum(cross2$Package=="R")==1) {
      #Start date when pkg was available, is when it is released
      start.date=cross1[k.pkg,  ]$Published  #start.date: start of period when pkg_vrs binary was avilable for this R-version
      #End date is either when then ext package version is released or the present.
      if (k.pkg<nrow(cross1)) end.date=as.DateYMD(cross1[k.pkg+1,]$Published)-2  #If already replaced, when it was replaced, minus 2 days for caution
      if (k.pkg==nrow(cross1))  end.date=as.DateYMD(Sys.Date())-2    #If not yet replaced, still current with today's MRAN, but use minus two days for caution


      #If end.date not yet in toc, update toc
      if (max(cran.toc$Published)<end.date) load.cran.toc(T)


    } #There is only 1 version of R within set
  }#R with after

  start.date=as.DateYMD(start.date)
  end.date=as.DateYMD(end.date)
  as.numeric(end.date-start.date)
  mid.date=as.numeric((end.date-start.date))/2+start.date


  return(mid.date)
}#End.function 2.11
