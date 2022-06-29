#' Load a specific version of groundhog, as available on a given date 
#' 
#'@param date character string  (yyyy-mm-dd), or date value, with the date which determines the 
#'version of groundhog to load 
#'@examples
#' \dontrun{
#' #Load groundhog as available on 2021-03-12 (v1.3.2)
#' meta.groundhog("2021-03-12")
#' }
#' 
#' @export


  meta.groundhog <- function (date)
      {
      #0 The package is groundhog
         pkg='groundhog'
  
      #1) Is date valid?
         validate.date(date) #Function defined in utils.R

      #2) Make a snowball, which is really just groundhog
          snowball <- get.snowball(pkg, date)

      #3) Install it
          install.snowball(snowball, date=date)
          
      #4) Unload groundhog 
          unloadNamespace('groundhog')
        
      #5) attach the requested version
         suppressPackageStartupMessages ( loadNamespace('groundhog', lib.loc = snowball$installation.path[1]))
         suppressPackageStartupMessages ( attachNamespace('groundhog'))
         
      #6) message
         vrs <- get.version('groundhog',date)
         message2("Now using 'groundhog_" , vrs , "'.")
         
      #7) Warning if version is prior ot 1.4 that meta is no longer available
         if (compareVersion(vrs,"1.4.0") <0) {
           message1("This version of groundhog does not have the 'meta.groundhog()' function.")
           message1("To switch to a different version of groundhog, you will have to restart ")
           message1("the R session reload groundhog' \n ",restart.text())
           
         }

      }