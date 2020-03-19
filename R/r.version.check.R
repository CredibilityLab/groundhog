#2.9 R Verson comparison
r.version.check=function(date)
{
  #Using
  r.using.major=  as.numeric(R.version$major)
  r.using.minor=  as.numeric(strsplit(R.version$minor,"\\.")[[1]][1])
  r.using.patch=  as.numeric(strsplit(R.version$minor,"\\.")[[1]][2])
  r.using.full=paste0(r.using.major,".",r.using.minor,".",r.using.patch)
  #need
  r.need.full=get.version("R",date)
  r.need.split=strsplit(r.need.full,"\\.")[[1]]
  r.need.major=as.numeric(r.need.split[1])
  r.need.minor=as.numeric(r.need.split[2])
  r.need.patch=as.numeric(r.need.split[3])
  return(namedList(r.using.full, r.using.major, r.using.minor, r.using.patch,
                   r.need.full, r.need.major, r.need.minor, r.need.patch))
}
