#If a package is being loaded with groundhog and we want to show a specific warning with an OK
#this function shows it with a prompt, if OK goes on, otherwise stops and in any case does not show
#warning again for some number of days


pkg_specific.warnings <- function(pkg)
  {
  #1 Set of packages with some warning
      all_pkgs_with_warnings <- c('foreach')

  #2 if pkg does not belong to set of packages with warnings, stop with early return
      if (!pkg %in% all_pkgs_with_warnings) return(invisible())

  
  #3. Messages
      msg=days=list()
      
    #3.1 - foreach
        msg[['foreach']] <- paste0('The package `foreach` can jeopardize reproducibility by loading packages\n',
                                  'from the default R library in background.\n',
                                  'This is easy to solve adding 3 lines of code to a foreach loop.  Check out:\n',
                                  'https://groundhogr.com/foreach')
  
        days[['foreach']] <- 90
        

    
  #4 Show warning with a prompt to continue -  function 28 in utils.R
      prompt.ok(prompt_name = 'pkg_foreach' , msg=msg[[pkg]], days_till_shown_again = days[[pkg]])    

    }


