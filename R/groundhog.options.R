#' Set an option for groundhog
#
#' @param name Character. The name of the option being set.
#' @param value Character or logical. The value to assign the option to.
#' 
#' @details
#' Current options that can be set manually include the following:
#' 
#' [os]: operating system. It is supposed to be detected automatically, but if
#' groundhog were to not correctly identify a user's 'os' it can be set manually.
#' Accepted options are 'windows','mac', 'mac_arm' (for the x1+ chip), 'linux'.
#'  
#' [download.sequentially]. To speed up the download of binaries, they are 
#' by default downloaded with 'libcurl' simultaneously, if 'libcurl' is detected.
#' A user can manually turn this off to avoid possible errors. This will lead to
#' downloading packages one a time, slowing down installations by a few seconds per 
#' package (& dependency).
#' @examples
#' \dontrun{
#' set.groundhog.option('download.sequentially',TRUE)
#' }
#'
#' @export
#'
#' @seealso [get.groundhog.option()]
#'
#'

#Function 1 set it
  set.groundhog.option <- function(name, value)
  {

  #1   #If not accepted error
       #Set in zzz.R
          if (!name %in% .pkgenv[['acceptable.option.names']]) {                     
             stop("groundhogg says: '",name,"' is not one of the accepted options. ",
             "\nThe set of accepted options is:\n",pasteQC(accepted.names))
            }
    
  #------------------------    
  #2 Possible values
      accepted.values <- list()
    
    #2.1 OS
        accepted.values[['os']] <-c('windows','mac','mac_arm','linux')
        
    #2.2 Skip libcurl in download
        accepted.values[['download.sequentially']] <- c(TRUE,FALSE)

  #----------------------------
        
  #3 Validate values
        
       if (!value %in% accepted.values[[name]]) {
         stop("   groundhog says: '",value,"' is not one of the accepted options for '",name,"'.\n",
              "   The set of accepted options is:\n",
              "   ",pasteQC(accepted.values[[name]]))
           } #End if not accepted value
      
  #----------------------------     
  #4 store it
        
    #Set it in environemnt
      .pkgenv[[name]] <- value
    
    #Save it to path
      path <-  paste0(path.expand("~"), "/R_groundhog/options/",name,".txt")
      dir.create(dirname(path), showWarnings = FALSE,recursive = TRUE)
      write (value,path)
      
    #Message
      message1("The value for '",os,"' has been set to '",value,"'")
      
    } #End of function

  
#-----------------------------------------------------
  
#' Get groundhog option
#' 
#' Obtain the value currently assigned to a manually set option.
#'
#' @examples
#' \dontrun{
#' get.groundhog.option("download.sequentially")
#' }
#'
#' @export
#'
#' @seealso [set.groundhog.option()]
#'
  
  get.groundhog.option <- function(name)
  {
   #1 Validate accepted options
    accepted.names <- c('os','download.sequentially')
    
    #If not accepted error
      if (!name %in% accepted.names) {
         stop("  groundhog says: '",name,"' is not one of the accepted options.\n",
              "    The set of accepted options is:\n",
              "   ",pasteQC(accepted.names))
      }
    
    #2 Error if not set 
       if (!exists(.pkgenv[[name]]))  {
          stop("    groundhog says: '",name,"' has not been assigned a value.")
       }
    
    #3 Read it
      return(.pkgenv[[name]])
  } #End of get.groundhog.option()
  
  
