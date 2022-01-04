#Functions to delete local files which may be corrupt


  delete.snowballs <- function()
    {
      snowballs_dir <- paste0(get.groundhog.folder(), '/snowballs/')
      if (file.exists(snowballs_dir)) {
         success.deleting <- file.remove(snowballs_dir)
         if (success.deleting==FALSE) {
            message1("groundhog could not delete the local snowballs,you may try to do so by hand\n",
                    "(perhaps close R to do so). Try deleting this directory:\n'",snowballs_dir,"'")
           } #End if success deleting
         
         if (success.deleting==TRUE) {
            message1("groundhog deleted all locally saved snowballs.")
         }
      } else {
        message1('There are no snowballs to delete')
      }
    }