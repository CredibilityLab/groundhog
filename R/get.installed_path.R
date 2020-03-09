#' Check if package version is installed for current or past R versions,

get.installed_path=function(pkg,vrs)
{
  #Get full paths
  pkg_search_paths=get.pkg_search_paths(pkg,vrs)

  #Search for package till found
  installed_path=""
  for (pathk in pkg_search_paths)
  {
    #If directory and file exists, test if package is installed, if file does not exist; it is not installed anyway
    if (file.exists(pathk)) {
      df.pkg=data.frame(installed.packages(lib=pathk))  #Note: in {groundhogR} each package version gets a 'library' within the R version
      if (nrow(df.pkg) >0) {
        installed_path=pathk
        break
      } #If file exists identified by possible search path
    } #End if found
  } #End:   for (pathk in
  return(installed_path)
}
