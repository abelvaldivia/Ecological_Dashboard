
## Clear Global Environment
rm(list = ls())

## Clear console
cat("\014")
 
 #Set WD Directory for MAC
  setwd("/Users/abelvaldivia/Dropbox/Collaborations/RARE/Fish Forever M&E/ME Framework and Metrics/Eco_DataPortal")
#Set WD for DELL 
  #setwd("C:/Users/avaldivia/Dropbox/Collaborations/RARE/Fish Forever M&E/ME Framework and Metrics/Eco_DataPortal")
    library(rsconnect)
      #Using AbelValdivia gmail account
      #rsconnect::setAccountInfo(name='abelvaldivia', token='5B3896627F68B3FD51DC17C5D5FB77D4', secret='/eFEcQKMN6aHhaNNxAjdF+3p9xT6G+4fRkY5lGkd')
        
      #Using Rare email account
      rsconnect::setAccountInfo(name='rare', token='8BE8DBA6075599151447359DB0D53B33',
                                secret='y5Qr5XfXe5j5kr8+s6Ajf0ji7EH6X65JWug6zZSr')
      ### Run Shiny App
       shiny::runApp()

       ### Publish to Rare server
       rsconnect::deployApp(account = "rare")
   
      dir()

  
      
 