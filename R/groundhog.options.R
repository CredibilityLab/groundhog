

  set.groundhog.option <- function(name, value)
  {

  #1 Validate accepted options
    accepted.names <- c('os')
    
    #If not accepted error
      if (!name %in% accepted.names) {
         stop("groundhogg says: '",name,"' is not one of the accepted options. ",
         "\nThe set of accepted options is:\n",pasteQC(accepted.names))
      }
    
  #------------------------    
  #2 Possible values
      accepted.values <- list()
    
    #2,1 OS
        accepted.values[['os']] <-c('windows','mac','mac_arm','linux')
        
    #2.2 NEXT OPTION - TBA
        
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
      write(path,value)
      
    #Message
      message1("The value for '",os,"' has been set to '",value,"'")
      
    } #End of function

  
  
  get.groundhog.option <- function(name)
  {
   #1 Validate accepted options
    accepted.names <- c('os')
    
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
  
  

