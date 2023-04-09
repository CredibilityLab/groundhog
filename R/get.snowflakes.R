
#returns a list where each element is a vector of package names


get.snowflakes = function(snowball, date) 
{
  
   #1 Keep source and remotes
     snowball <- snowball[snowball$from %in% c('source', 'GitHub','GitLab'),]
        
   #2 Start empty 
        
      dep12 <- data.frame(pkg=character() , dep2=character())
      
      
   #3 Populate with with all pkgs in the snowball
      for (pkgk in snowball$pkg)
        {
        if (!pkgk %in% dep12$pkg) 
          {
          dep12.k <- get.all.dependencies(pkgk, date)
          dep12.k <- dep12.k [!dep12.k$pkg %in% dep12$pkg,]
          dep12 <- rbind(dep12, dep12.k)
          }
      }
              
    #4 Keep only packages that are in the snowball
      dep12 <- dep12[dep12$pkg %in% snowball$pkg,]
      dep12 <- dep12[dep12$dep2 %in% snowball$pkg,]

              
    #5 Drop base packages
      dep12<-dep12[!dep12$pkg %in% base_pkg() & !dep12$dep2 %in% base_pkg(), ]
            
    #6 Break into snowflakes  
      k<-0
      snowflakes<-list()
      while (nrow(dep12) > 0) {
        
      #Counter
        k <- k + 1
                
      #Find dependencies without dependencies  TRUE/FALSE vector
        indep.rows <- !(dep12$dep2 %in% dep12$pkg)
                
      #Make snowflake #k
        indepk <- unique(as.character(dep12$dep2[indep.rows]))
        snowflakes[[k]] <- indepk
                  
      # Drop those rows from both
        dep12 <- dep12[!indep.rows, ]
                  
       # Safety valve in case loop impossible to end
          if (k == 50000) break 
      
        
        }    
      
    #7 Add anything in snowball that is not in dep12 (these are packages without dependencies)
      flat_snowflakes <- unlist(snowflakes)
      if (sum(!snowball$pkg %in% flat_snowflakes)>0) {
          snowflakes[[k+1]] <-  snowball$pkg[!snowball$pkg %in% flat_snowflakes]
          }
      
      
      
      
    #8 Drop already installed packages from the snowflakes
      for (k in 1:length(snowflakes))
        {
         snowflakes[[k]] <-snowflakes[[k]] [snowflakes[[k]] %in% snowball$pkg[snowball$installed==FALSE]  ]
        }
        
    #9 Drop empty snowflakes        
      snowflakes <- snowflakes[lapply(snowflakes,length)>0]   
  
  
    #8 Return
      return(snowflakes)

    }