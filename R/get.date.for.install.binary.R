get.date.for.install.binary=function(pkg_vrs)
{
  binary.date=as.DateYMD("1970-01-01")
  #R being used
  r.using.full =get.rversion()  #Get current
  r.using.major=R.version$major
  r.using.minor=strsplit(R.version$minor,"\\.")[[1]][1]

  #Get R Toc
  R.toc=toc("R")          #Get R toc

  #Extract  major,minor,patch to  R versions in toc
  R.toc$major=sapply(  strsplit(R.toc$Version,"\\."), `[`, 1)
  R.toc$minor=sapply(  strsplit(R.toc$Version,"\\."), `[`, 2)
  R.toc$path=sapply(  strsplit(R.toc$Version,"\\."), `[`, 3)

  #Find first and last current major.minor
  R.same.minor=R.toc[R.toc$major == r.using.major & R.toc$minor == r.using.minor, ]
  k0=match(R.same.minor[1,]$Version,R.toc$Version)
  k1=match(R.same.minor[nrow(R.same.minor),]$Version,R.toc$Version)

  #Find exact match, and split from rest, to start with perfect match
  k.same=match(get.rversion(),R.toc$Version)
  k.others=k1:k0
  k.others=k.others[k.others!=k.same]

  #Loop over them
  for (k in c(k.same,k.others))
  {
    R_vrs=R.toc[k,1]
    binary.date=get.R.pkg.date(pkg_vrs=pkg_vrs,R_vrs=R_vrs)
    if (binary.date>"1970-01-01") break
  }
  #If date if from before MRAN's first day, 2014-09-17, go to 1970
  if (binary.date<"2014-09-17") binary.date=as.DateYMD("1970-01-01")
  return(as.DateYMD(binary.date))
}
