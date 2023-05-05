 message.batch.installation.feedback <- function(snowball, snowflakes,  k , cores)
  {
  #0 Log file path
        log_path <- file.path(get.groundhog.folder(),"batch_installation_log.txt")
    
  #1. Get installation time for this batch 
    sec.k <- get.parallel.time(snowball$installation.time[snowball$pkg %in% snowflakes[[k]] ] , cores) #utils.R #44
  
  #2  Get total remaining time
          if (k < length(snowflakes))
            {
            sec.tot=0
              for (j in k:length(snowflakes)) 
              {
              sec.tot <- sec.tot + get.parallel.time(snowball$installation.time[snowball$pkg %in% snowflakes[[j]] ] , cores) #utils.R #44
              } 
            } else {
              sec.tot =  sec.k
            }
    
  #3 Translate to H:M
     now             <- format(Sys.time(), "%H:%M")
     estimate.batch  <- format(Sys.time() + sec.k, "%H:%M")
     estimate.tot    <- format(Sys.time() + sec.tot, "%H:%M")
          
  #4 Print message on console
      if (k==1) {
          message2("\n\nWill now install ", nrow(snowball)," packages from source files, in " , length(snowflakes), " batches.")
          message1("   Packages within a batch are installed in parallel.")
          message1("   Batches are installed sequentially.\n")
          message1("   As of ",now, " all ", nrow(snowball), " packages are expected to finish installing around ",estimate.tot, ".")
          message1("   (but this is a noisy estimate).")
        }
            
          
    #4.1 Batch counter
            n.batch = length(snowflakes[[k]])
            message1("\n\n------------------------------------------------------")
            message2("Batch " , k , " of ",length(snowflakes)," has " , n.batch , " packages.")
                  
    #4.2 set of packages
            #Message 
                #list with multiple files
                  if (n.batch> 1 ) {
                      message1("  They are: ", pasteQC(sort(snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ])))
                               }
                
                #Single file
                  if (n.batch==1) {
                     message1("  It is: ", snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ])
                  }

    #4.3 how much time  left 
           
            #Always say how long this batch      
               msg <- paste0("  As of ",now, " this batch is expected to finish installing by ",estimate.batch)
                                      
            #Say also total, unless this is the last batch
               if (k<length(snowflakes)) {
                msg <-paste0(msg, " and all batches by ",estimate.tot,".")
                }
            
            #Now show it
              message1(msg)
              message1("  (but these are noisy estimates).")
        
    #5 Installation feedback saved to txt file
          #line of text
            #About this batch
            log_line<-paste0(
                      "Processing batch #" , k , " of ",length(snowflakes),"\n",
                      "    Batch contains " , n.batch , " packages.\n",
                      "    As of ",now, " it is expected to finish installing around ",estimate.batch,"\n")
            
            #About the last batch
            if (k<length(snowflakes)) {
                log_line <- paste0(log_line, 
                      "    and it is expected that all batches will finish around ",estimate.tot,".\n")
            }
            
            #Imprecise estimates
              log_line<-paste0(log_line, "    But these are imprecise estimates.\n")
            
            #Package list
              log_line<-paste0(log_line,"\n\nThe ",n.batch," packages in this batch are:\n", 
                               pasteQC(sort(snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ])),
                                         "\n\n--------------------------------------------------------------------------------")
              
              
                
            #Save         
              path_installation_log <- file.path(get.groundhog.folder(),"install_progress.txt")
              
              #initiate file
              
              if (k==1) {
                #Opening line of file
                    start_line <- paste0("Groundhog Installation Progress File\n",
                                         "(Created: ",Sys.Date(),")\n\n")
                #Save it
                    write(start_line , path_installation_log,append=FALSE)
                  }
              #Save this new line
                write(log_line,path_installation_log,append=TRUE)
              
            #Tell them about the file
                message1("\n\nYou can keep trak of installation progress and expected \n",
                         "completion time opening this real-time updated text file:")
                
            #Make backwards slashes for windows
                edited_path <- path_installation_log
                if (get.os()=='windows') {
                  edited_path <- gsub( "/", "\\\\", edited_path)
                }
            #Show the path in bold
                message2(edited_path)
              
            #Close box
              message1("\n----------------------------------------------------------")
              
              
         #Wait 3 seconds if k==1
              if (k==1) Sys.sleep(5)
   
            
    } #End function