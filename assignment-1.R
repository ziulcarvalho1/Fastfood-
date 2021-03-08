##This program indicates which weather parameter works better as a predictor for demand for each type of product
##We identify that for each one of the 9 combinations of day of week and month of the year
############################################################################################################
## Load the necessary libraries
library(caTools)
library(stringr)
## Load the database
setwd("/Curso-ML/Assignment-1/")
##data0 <- read.csv("total-sp-11.csv")
##linear1x <- lm(formula=data[[26]] ~ data[[12]])
##linear2x <- cor(data[[26]], data[[12]])
##print(linear1x)
##print(coef(linear1x))
##print(linear2x)
data0 <- read.csv("data-06.csv") 

## Cleaning the data
## Here we check if the columns tempmin, tempmax, tempmed, humidit and insulation are 0
## If they are we replace the value for the meam
tempmax    <-data0[[11]]
tempmin    <-data0[[12]]
tempmed    <-data0[[13]]
umidade    <-data0[[14]]
insolacao  <-data0[[15]]

media1 <- mean(tempmax, trim = 0, na.rm = TRUE)
media2 <- mean(tempmin, trim = 0, na.rm = TRUE)
media3 <- mean(tempmed, trim = 0, na.rm = TRUE)
media4 <- mean(umidade, trim = 0, na.rm = TRUE)
media5 <- mean(insolacao, trim = 0, na.rm = TRUE)

lenxxx <-nrow(data0)
for (i in 1:lenxxx){
  if (data0[i,11]==0){
    data0[i,11]<-media1
  }
  if (data0[i,12]==0){
    data0[i,12]<-media2
  }
  if (data0[i,13]==0){
    data0[i,13]<-media3
  }
  if (data0[i,14]==0){
    data0[i,14]<-media4
  }
  if (data0[i,15]==0){
    data0[i,15]<-media5
  }

  
}
  


## Separate the data into trainning data and test data
data_sample = sample.split(data0,SplitRatio=0.95) ##here we separate the file into trainning and test data
dataxx = subset(data0,data_sample==TRUE) ## General trainning data
data1 = subset(data0,data_sample==FALSE) ##test data
##dataxx <- data0
##data1  <- data0
## Drop not used columns
##print(data1)
data1$cbmaker   <- NULL
data1$combos    <- NULL
data1$kit2      <- NULL
data1$kit1      <- NULL
data1$promotion <- NULL
##print(data1)

len1 <- nrow(data1)
##print(len1)
## Assign 0 to dimensioned variables ax,bx and dx where A and B are the Ax = B = Y in a linear regress
## Dx is the weather parameter selected to be used as reference - the one identified with the bigger correlation
ax<-matrix(, nrow=1, ncol=27)
bx<-matrix(, nrow=1, ncol=27)
ex<-matrix(, nrow=1, ncol=27)
Dx<-matrix(, nrow=1, ncol=27)
##print(Dx)           
for (ss1 in 1:27){
  ax[1,ss1]<-0
  bx[1,ss1]<-0 
  Dx[1,ss1]<-0
}

class(ax)
##print(ax)
##print(bx)
## this for loop goes through every row in the testdata
for (register in 1:len1){
##  print(data1[register,])
  dia <-data1$diasem[register]
  mes1 <-data1$mes[register]
##  print(register)
##  print(dia)
##  print(mes1)
  ## now we have to segment the trainning data into 9 subgroups by week and by month
  data <- subset.data.frame(dataxx, diasem == dia & mes == mes1) ##subset trainning week and month
  data$cbmaker   <- NULL
  data$combos    <- NULL
  data$kit2      <- NULL
  data$kit1      <- NULL
  data$promotion <- NULL
  datazzz <- data
  
  
  print(data)
  ##dim(dataxx)
  
  ##print(data)
  ##print(data1)
  ##summary(data)
  ##summary(data1)
  len3=nrow(data)
  ##print(len3)
  
  precipitac <-data[[10]]
  tempmax    <-data[[11]]
  tempmin    <-data[[12]]
  tempmed    <-data[[13]]
  umidade    <-data[[14]]
  insolacao  <-data[[15]]
  
  data$cbmaker   <- NULL
  data$combos    <- NULL
  data$kit2      <- NULL
  data$kit1      <- NULL
  data$promotion <- NULL
  
  desserts  <-data[[19]]    
  pizzas    <-data[[20]]  
  beverage  <-data[[21]]   
  sfiha     <-data[[22]]   
  snack     <-data[[23]]    
  pastas    <-data[[24]]     
  dishes    <-data[[25]]      
  savory    <-data[[26]]   
  salads    <-data[[27]]
  
  print(data)
  z1 = 19
  z2 = 10
  
  while (z1<28){
    maiorrs     <- -10000
    melhoropcao <- 0
    
    #### Bell curve - tail elimination - Here we remove from the file the outliners
    ####
    data <- datazzz
    len1zzz <- nrow(data)
##    print(len1zzz)
   
    
    for (varzzz in z1:z1) {
      maiorzzz <- 0
      menorzzz <- 1000000
      
      for (registerzzz in 1:len1zzz){
##        print(varzzz)
##        print(registerzzz)
##        print(data[registerzzz,varzzz])
        
        if (maiorzzz < data[registerzzz,varzzz]){
          maiorzzz <- data[registerzzz,varzzz]
        }
        if (menorzzz > data[registerzzz,varzzz]){
          menorzzz <- data[registerzzz,varzzz]
        }
        
      }
##      print("Maior")
##      print(varzzz)
##      print(maiorzzz)
##      print("Menor")
##      print(varzzz)
##      print(menorzzz)
      ## make the normal selection
      rangexzzz <- maiorzzz-menorzzz
##      print(rangexzzz)
      passozzz <- rangexzzz/20 ##divided into 20 ranges
 ##     print(passozzz)
      faixazzz <- menorzzz
      
      for (faixa1zzz in 1:20){
        
        
        yzzz <-0
        for (register1zzz in 1:len1zzz){
          if (data[register1zzz,varzzz]>faixazzz & data[register1zzz,varzzz]<(faixazzz+passozzz)){
            yzzz <- yzzz+1
          }
        }
        if ((yzzz/len1zzz)<0.01){ ## minimum % to justify elimination of the sample
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
              data$ifood[register2zzz] <- 1 
              data$call[register2zzz]<-var
              data$android[register2zzz]<-yzzz
              data$iphone[register2zzz]<-register2zzz
              data$web[register2zzz]<-faixazzz
              ##delete registro
            }
          }
        }
        faixazzz <- faixazzz+passozzz
        
      } ## passa faixa1 a faixa1 1 a 20
      
      
    } ## muda de coluna a coluna
    ##print(datax)
    ##print("numero de elimiacoes")
    ##print(nrow(data))
    data <- subset.data.frame(data, ifood == 0) ##subset trainning week and month
    
    ## print(nrow(data))
    
    #### End of the bell routine - outliners elimination
    
    
    while (z2<16){
      
      var1 <-data[[z1]]
      var2 <-data[[z2]]
      ###print(z1)
      ###print(z2)
      ##    print(var1)
      ##    print(var2)
      ##class(var1)
      ##class(var2)
      
      linear <- lm(formula=var2 ~ var1)
      summary(linear)
      RS <- cor(var2,var1,method="pearson") ##summary(linear)$r.squared
      ##print(RS)
      if (RS >= maiorrs) { ###Here we select which one of the six factors is better to work as predictors
        maiorrs=RS
        melhoropcao = z2
        
        ##print(z2) 
      }
      ##scatter.smooth(x=var1, y=var2, main="temp min ~ sfiha")  # scatterplot
      ##
      ##print(linear)
      ##R-squared (R2) is a statistical measure that represents the proportion of the variance for a 
      ##dependent variable that's explained by an independent variable or variables in a regression model.
      ##Whereas correlation explains the strength of the relationship between an independent and dependent 
      ##variable, R-squared explains to what extent the variance of one variable explains the variance of the 
      ##second variable. So, if the R2 of a model is 0.50, then approximately half of the observed variation 
      ##can be explained by the model's inputs.
      ##R-squared indicates the percentage of the samplas fall into the regression line
      ##the bigger the R-Squared the better the prediction is
      ##We are calculating six R-squared and doing that we idenfiy which one of the six weather parameters 
      ##is better to be used do predict the demand
      
      z2 = z2+1
    }
##    print(z1)
##    print(melhoropcao)
    
    linearx <- lm(formula=data[[z1]] ~ data[[melhoropcao]])
   
    Cx <-coef(linearx)
    Ex <- confint(linearx, "data[[melhoropcao]]",level = 0.95)
    print(linearx)
    print(Ex)
    print("coef")
    print(Cx[[1]])
    Bx <- Cx[[1]]
    Ax <- Cx[[2]]
    
   ## print("novos valores")
   ## print(Cx)
   ## print(Ax)
  ##  print(Bx)
    ex[1,z1]<-Ex[2]
    ax[1,z1]<-Ax
    bx[1,z1]<-Bx
    Dx[1,z1] <- melhoropcao
  ##  print(z1)
    
    
    
    
    ## A and B are the parameters AX + B = Y where the X is the chosen weather parameter and Y is the price
    ## Now we have to apply this to the test dataset for this specifi value of Z1 (type of product)
    melhoropcao <- 0
    z2 <- 10
    z1 <- z1+1
    ##print(z1)
  }
  ## quant19 <- (ax[19]*data1[[ax[1,1]])+bx[1,19]
  ## print(ax[1,1])
  ## print(quant19)
  ## quant20 <- (ax[1,20]*data1[[ax[1,1]])+bx[1,20]
  ## quant21 <- (ax[1,21]*data1[[ax[1,1]])+bx[1,21]
  ## quant22 <- (ax[1,22]*data1[[ax[1,1]])+bx[1,22]
  ## quant23 <- (ax[1,23]*data1[[ax[1,1]])+bx[1,23]
  ## quant24 <- (ax[1,24]*data1[[ax[1,1]])+bx[1,24]
  ## quant25 <- (ax[1,25]*data1[[ax[1,1]])+bx[1,25]
  ## quant26 <- (ax[1,26]*data1[[ax[1,1]])+bx[1,26]
  ## quant27 <- (ax[1,27]*data1[[ax[1,1]])+bx[1,27]
  ##print(ax)
  ##print(bx)
  ##print(Dx)
  ##print(register)
  ##print(data1[register,])
  ##print(ax[1,19])
  ##print(bx[1,19])
  ##print(Dx[1,19])
  ##print(data1[register,Dx[1,19]])
  quant19 <- (ax[1,19]*data1[register,Dx[1,19]])+bx[1,19]
  ##print(quant19)
  ##print(data1[register,20])
  ##print(ax[1,20])
  ##print(bx[1,20])
  ##print(Dx[1,20])
  ##print(data1[register,Dx[1,19]])
  quant20 <- (ax[1,20]*data1[register,Dx[1,20]])+bx[1,20]
  ##print(quant20)
  ##print(data1[register,20])
  quant21 <- (ax[1,21]*data1[register,Dx[1,21]])+bx[1,21]
  quant22 <- (ax[1,22]*data1[register,Dx[1,22]])+bx[1,22]
  quant23 <- (ax[1,23]*data1[register,Dx[1,23]])+bx[1,23]
  quant24 <- (ax[1,24]*data1[register,Dx[1,24]])+bx[1,24]
  quant25 <- (ax[1,25]*data1[register,Dx[1,25]])+bx[1,25]
  quant26 <- (ax[1,26]*data1[register,Dx[1,26]])+bx[1,26]
  quant27 <- (ax[1,27]*data1[register,Dx[1,27]])+bx[1,27]
  data1$dessert1[register] <-quant19
  data1$pizzas1[register]  <-quant20
  data1$beverage1[register]<-quant21
  data1$sfiha1[register]   <-quant22
  data1$snack1[register]   <-quant23
  data1$pastas1[register]  <-quant24
  data1$dishes1[register]  <-quant25
  data1$savory1[register]  <-quant26
  data1$salads1[register]  <-quant27
  ##print(register)
  ##print(data1$desserts[register])
  ##print((data1$desserts[register]-quant19))
  ##print(abs((data1$desserts[register]-quant19)/data1$desserts[register]))
  data1$desserte[register] <- abs((data1$desserts[register]-quant19)/data1$desserts[register])
  data1$pizzase[register]  <- abs((data1$pizzas[register]-quant20)/data1$pizzas[register])
  data1$beveragee[register]<- abs((data1$beverage[register]-quant21)/data1$beverage[register])
  data1$sfihae[register]   <- abs((data1$sfiha[register]-quant22)/data1$sfiha[register])
  data1$snacke[register]   <- abs((data1$snack[register]-quant23)/data1$snack[register])
  data1$pastase[register]  <- abs((data1$pastas[register]-quant24)/data1$pastas[register])
  data1$dishese[register]  <- abs((data1$dishes[register]-quant25)/data1$dishes[register])
  data1$savorye[register]  <- abs((data1$savory[register]-quant26)/data1$savory[register])
  data1$saladse[register]  <- abs((data1$salads[register]-quant27)/data1$salads[register])
  ##print(data1[register,])
}
print(mean(data1$desserte))
print(mean(data1$pizzase))
print(mean(data1$beveragee))
print(mean(data1$sfihae))
print(mean(data1$snacke))
print(mean(data1$pastase))
print(mean(data1$dishese))
print(mean(data1$savorye))
print(mean(data1$saladse))
print(data1)
write.csv(data1,'/Curso-ML/Assignment-1/result-06.csv')
print(data1[,22])
print(data0)