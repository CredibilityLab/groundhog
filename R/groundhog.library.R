#' @export
# 8  Final function:   groundhog.library()
#' @importFrom utils capture.output
groundhog.library=function(pkg, date,
                           quiet.install=TRUE,            #Run install.packages() with quiet=TRUE?
                           plot.console=TRUE,             #Should information on installation time left be printed in Plots dialogue whe installing from source>
                           include.suggests=FALSE,        #Should suggested packages be installed?
                           current.deps=c("Rcpp"),        #Dependencies that will install current version regardless of gdate
                           ignore.package.conflicts=FALSE,#With TRUE, if an already attached package will be installed,it is detached, but all depednecies are left installed, with default, FALSE, script stops and asks for session restart
                           force.source=FALSE,            #TRUE will skip CRAN and MRAN attempts, download tar.gz, and install from it
                           force.install=FALSE)           #Even if package is found for this R-build, it will reinstall it.
{
  #8.1 Load cran.toc if not yet loaded
  if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)

  #8.2 Update cran.toc() if needed for entered date (#2.12)
  update_cran.toc_if.needed(date)

  #8.3 Check for r.mismatch (#2.13)
  check.mismatch.R(date)

  #8.4 Get vrs
  vrs=get.version(pkg,date)
  pkg_vrs=paste0(pkg,"_",vrs)

  #8.5 GET SNOWBALL (#6)
  snowball=get.snowball(pkg,date,include.suggests)

  #8.6 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
  if (ignore.package.conflicts==FALSE) check.snowball.conflict(snowball)

  #8.7 Install pacakges if needed
  install.snowball(pkg,date, include.suggests, force.install=force.install,force.source=force.source,plot.console=plot.console,quiet.install=quiet.install)

  #8.8 Do library() call for pkg_vrs
  library(pkg,character.only=TRUE)

  #8.9  verify success
  #pkg_vrs we wanted to load
  pkg_vrs=paste0(pkg,"_",vrs)
  #load active packagse
  active=paste0(capture.output(utils::sessionInfo()),collapse=",")
  #Found there?
  pos=regexpr(pkg_vrs,paste0(active))
  cat2()
  if (pos>0)   cat1(paste0("Succesfully loaded ",pkg_vrs," and its ",nrow(snowball)-1," dependencies."))
  if (pos==-1) cat1(paste0("FAILED to load ",pkg_vrs))

} #End of groundhog.library()
