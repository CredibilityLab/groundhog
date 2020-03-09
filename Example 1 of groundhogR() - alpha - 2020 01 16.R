      

rm(list = ls())

#Alpha version groundhogR - written by Uri Simonsohn (urisohn@gmail.com)
      source("C:/Dropbox/groundhogR/package itself/groundhogR Alpha 2020 02 28.R")
     #Eventually a CRAN package loaded with library(), for now source it:
#Example 1 - REcovering broken code with backwards incompatible change in {dplyr}
#      With version 0.5 {dplyr} changed how a command worked, leading to dropped variables
  
#Say you have data with repeat values of x 
         df1=data.frame(x1=c(1,1,2),x2=c('a','b','c'))
    
#To drop repeated rows you can use the package 'dplyr' and run
        #distinct(df1,x1) 
         
#The problem is that in June 2016, the function dplyr::distinct() was modified in 
#a non backwards-compatible way. Up to that date it would keep all variables but,
#afterward the SAME fucntion drops all other variables,
#The same line of code, run in the same computer, on the same date, has one effect
#before june 2016 vs after
         
#With groundhogR the script tells us which version of tldy to use keeping the same results
         
         
# if before the change, then both variables are kept
      groundhog.library('dplyr',date="2016-06-22")
      distinct(df1,x1) 
      #here we keep both variables, x1 & x2
      
        
# if after the change, only x1 is kept
#Now as it was two days after 
      #IMPORTANT: since another version of dplyr is loaded, we do CTRL-SHIFT-F10 to restart the R Session and load the newer version
      groundhog.library('dplyr',date="2016-06-26")
      distinct(df1,x1) 
      #here we keep only x1 variables

