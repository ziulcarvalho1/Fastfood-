## Load the necessary libraries
library(caTools)
library(stringr)
## Load the database
setwd("/Curso-ML/Assignment-1/")
data <- read.csv("data-06.csv")
datax <- data
print(data[34,1])
len1zzz <- nrow(data)
print(len1zzz)
precipitac <-data[[10]]
tempmax    <-data[[11]]
tempmin    <-data[[12]]
tempmed    <-data[[13]]
umidade    <-data[[14]]
insolacao  <-data[[15]]





desserts  <-data[[19]]    
pizzas    <-data[[20]]  
beverage  <-data[[21]] 
data$cbmaker   <- NULL
data$combos    <- NULL
sfiha     <-data[[22]]
data$kit2      <- NULL
data$kit1      <- NULL

snack     <-data[[23]]    
pastas    <-data[[24]]     
dishes    <-data[[25]]
data$promotion <- NULL
savory    <-data[[26]]   
salads    <-data[[27]]

for (varzzz in 19:19) {
 maiorzzz <- 0
 menorzzz <- 1000000
 
 for (registerzzz in 1:len1){
   print(varzzz)
   print(registerzzz)
   print(data[registerzzz,varzzz])
   
   if (maiorzzz < data[registerzzz,varzzz]){
     maiorzzz <- data[registerzzz,varzzz]
   }
   if (menorzzz > data[registerzzz,varzzz]){
     menorzzz <- data[registerzzz,varzzz]
   }

 }
 print("Maior")
 print(varzzz)
 print(maiorzzz)
 print("Menor")
 print(varzzz)
 print(menorzzz)
 ## make the normal selection
 rangexzzz <- maiorzzz-menorzzz
 print(rangexzzz)
 passozzz <- rangexzzz/20
 print(passozzz)
 faixazzz <- menorzzz
 
 for (faixa1zzz in 1:20){
  
 
  yzzz <-0
  for (register1zzz in 1:len1zzz){
    if (data[register1zzz,varzzz]>faixazzz & data[register1zzz,varzzz]<(faixazzz+passozzz)){
      yzzz <- yzzz+1
    }
  }
  if ((yzzz/len1zzz)<0.01){
   for (register2zzz in 1:len1zzz){
     if (data[register2zzz,varzzz]>faixazzz & data[register2zzz,varzzz]<(faixazzz+passozzz)){
       print("superior faixa")
       print(faixazzz+passozzz)
       print("inferior faixa")
       print(faixazzz)
       print("Number of events in this range")
       print(yzzz)
       print("percentage of events in this range")
       print(yzzz/len1zzz)
       print(data[register2zzz,varzzz])
       print("deletei registro")
       datax$ifood[register2zzz] <- 1 
       datax$call[register2zzz]<-var
       datax$android[register2zzz]<-yzzz
       datax$iphone[register2zzz]<-register2zzz
       datax$web[register2zzz]<-faixazzz
       ##delete registro
     }
    }
   }
  faixazzz <- faixazzz+passozzz
 
  } ## passa faixa1 a faixa1 1 a 20
 
 
} ## muda de coluna a coluna
##print(datax)
datax <- subset.data.frame(datax, ifood == 1) ##subset trainning week and month
write.csv(datax,'/Curso-ML/Assignment-1/result-bell.csv')
  