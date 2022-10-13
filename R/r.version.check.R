# Compare \R versions
#
# Compare the current R version to the \R version at a given `date`.`

r.version.check <- function(date) {
  #If using an R version not in the cran.toc.rds give error
 
  # Using
    r.using.major <- as.numeric(R.version$major)
    r.using.minor <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][1])
    r.using.patch <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][2])
    r.using.majmin <- paste0(r.using.major, ".", r.using.minor)
    r.using.full <- paste0(r.using.majmin, ".", r.using.patch)
    
    
  # need
    r.need.full <- as.character(get.version("R", date, patch = "max"))
    r.need.split <- strsplit(r.need.full, "\\.")[[1]]
    r.need.major <- as.numeric(r.need.split[1])
    r.need.minor <- as.numeric(r.need.split[2])
    r.need.majmin <- paste0(r.need.major, ".", r.need.minor)
    r.need.patch <- r.need.split[3] # version 2.15.1-w generates NA by coercion

  return(namedList(
    r.using.full, r.using.major, r.using.minor, r.using.majmin, r.using.patch,
    r.need.full, r.need.major, r.need.minor, r.need.majmin, r.need.patch
  ))
}

