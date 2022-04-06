#' Re-enable groundhog.library() to try MRAN binaries
#'
#' When an install from MRAN fails, groundhog does not try MRAN again for 5 hours
#' assuming the server is down. You can over-rule this preventive measure by running
#' this function.
#' @export

mran.is.up <- function()
{
  mran.is.down_path <-file.path(get.groundhog.folder(),'mran.is.down.txt')
  if (file.exists(mran.is.down_path)) {
    #If file exists
      unlink(mran.is.down_path)
      message1("groundhog.library() will again attempt to download from MRAN")
    } else {
      message1('MRAN downloads are not currently disabled; nothing has changed.')
      
    }
 

}

