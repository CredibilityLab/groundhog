


get.snowflakes = function(snowball, date) 
{
  
   #1 Keep only source
     snowball <- snowball[snowball$from == 'source',]
        
   #2 Start empty 
        
      dep12 <- data.frame(pkg=character() , dep2=character())
                  
   #3 Seed with with all pkgs in the snowball
      for (pkgk in snowball$pkg)
        {
        if (!pkgk %in% dep12$pkg) 
          {
          dep12.k <- get.all.dependencies(pkgk, date)
          dep12.k <- dep12.k [!dep12.k$pkg %in% dep12$pkg,]
          dep12 <- rbind(dep12, dep12.k)
          }
      }
                      
    #4 Drop base packages
      dep12<-dep12[!dep12$pkg %in% base_pkg() & !dep12$dep2 %in% base_pkg(), ]
            
    #5 Break into snowflakes  
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
                
 
    #6 Drop already istalled packages from the snowflakes
      for (k in 1:length(snowflakes))
        {
         snowflakes[[k]] <-snowflakes[[k]] [snowflakes[[k]] %in% snowball$pkg[snowball$installed==FALSE]  ]
        }
        
    #7 Drop empty snowflakes        
      snowflakes <- snowflakes[lapply(snowflakes,length)>0]   
  
  
    #8 Return
      return(snowflakes)

    }