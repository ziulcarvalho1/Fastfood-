## This program predicts the sales of each product based on the weather parameters
## The program identifies which parameter works better as a predictor for demand for each type of product
## The program also groups the sales by type of day of week and type of the month of the year
############################################################################################################
## This is the version to work with the shiny app
###########################################################################################################
## Load the necessary libraries
library(caTools)
library(stringr)

## set the directory and load the database
setwd("/Curso-ML/Assignment-1/")
data0 <- read.csv("data-06.csv") 

## Cleaning the data fase -1 
## Here we check if the columns tempmin, tempmax, tempmed, humidit and insulation are 0
## If they are we replace the value for the meam

tempmax    <-data0[[11]]
tempmin    <-data0[[12]]
tempmed    <-data0[[13]]
umidade    <-data0[[14]]
insolacao  <-data0[[15]]

media1 <- mean(tempmax,   trim = 0, na.rm = TRUE)
media2 <- mean(tempmin,   trim = 0, na.rm = TRUE)
media3 <- mean(tempmed,   trim = 0, na.rm = TRUE)
media4 <- mean(umidade,   trim = 0, na.rm = TRUE)
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

## Trainning data 
dataxx <- data0

## Assign initial value of zero to dimensioned variables ax,bx and dx where A and B are the Ax = B = Y in a linear regress
## Dx is the weather parameter selected to be used as reference - the one identified with the bigger correlation
ax<-matrix(, nrow=1, ncol=27)
bx<-matrix(, nrow=1, ncol=27)
Dx<-matrix(, nrow=1, ncol=27)
data1 <- matrix(,nrow=1,ncol=27)
           
for (ss1 in 1:27){
  ax[1,ss1]<-0
  bx[1,ss1]<-0 
  Dx[1,ss1]<-0
  data1[1,ss1]<-0
}

class(ax)
  ## these are the values gotten from the shimy app
  dia  <- 1
  mes1 <- 1
  data1[10]    <-   50
  data1[11]    <-  30
  data1[12]    <-  15
  data1[13]    <-  20
  data1[14]    <-  70
  data1[15]    <-  2.4

  ## now we have to segment the trainning data into 9 subgroups by week and by month
  data <- subset.data.frame(dataxx, diasem == dia & mes == mes1) ##subset trainning week and month
  ## Drop some colunms that will not be used
  data$cbmaker   <- NULL
  data$combos    <- NULL
  data$kit2      <- NULL
  data$kit1      <- NULL
  data$promotion <- NULL
  
  datazzz <- data
  len3=nrow(data)

  desserts  <-data[[19]]    
  pizzas    <-data[[20]]  
  beverage  <-data[[21]]   
  sfiha     <-data[[22]]   
  snack     <-data[[23]]    
  pastas    <-data[[24]]     
  dishes    <-data[[25]]      
  savory    <-data[[26]]   
  salads    <-data[[27]]
  
  z1 = 19
  z2 = 10
  
  while (z1<28){
    
    maiorrs     <- -10000 ## initial values of correlation
    melhoropcao <- 0      ## initial value best option - weather parameter with bigger correlation
    
    #### cleaning the data - fase -2 -
    #### Bell curve - tail elimination - Here we remove from the file the outliners 
    ####
    data <- datazzz
    len1zzz <- nrow(data)
  
    
    for (varzzz in z1:z1) {
      maiorzzz <- 0
      menorzzz <- 1000000
      
      for (registerzzz in 1:len1zzz){

        if (maiorzzz < data[registerzzz,varzzz]){
          maiorzzz <- data[registerzzz,varzzz]
        }
        if (menorzzz > data[registerzzz,varzzz]){
          menorzzz <- data[registerzzz,varzzz]
        }
        
      }

      ## make the normal selection
      rangexzzz <- maiorzzz-menorzzz ## range = (max value - min value)
      passozzz <- rangexzzz/20       ## range divided into 20 ranges = step (passos)
      faixazzz <- menorzzz
      
      for (faixa1zzz in 1:20){ ## faixa1 goes from min value to max increasing a step each cycle
        
        
        yzzz <-0
        for (register1zzz in 1:len1zzz){
          if (data[register1zzz,varzzz]>faixazzz & data[register1zzz,varzzz]<(faixazzz+passozzz)){
            yzzz <- yzzz+1
          }
        }
        if ((yzzz/len1zzz)<0.01){ ## minimum % to justify elimination of the sample
          for (register2zzz in 1:len1zzz){
            if (data[register2zzz,varzzz]>faixazzz & data[register2zzz,varzzz]<(faixazzz+passozzz)){
              data$ifood[register2zzz] <- 1 
              data$call[register2zzz]<-var
              data$android[register2zzz]<-yzzz
              data$iphone[register2zzz]<-register2zzz
              data$web[register2zzz]<-faixazzz
              ##delete registro
            }
          }
        }
        faixazzz <- faixazzz+passozzz ## passo = step
        
      } ## goes from range 1 to range 20 (faixazz 1 to 20)
      
      
    } ## muda de coluna a coluna

    data <- subset.data.frame(data, ifood == 0) ##subset trainning week and month
    
    #### End of the bell routine - outliners elimination
    ### Here we are going to check the correlation of each weather parameters with each product and select 
    ### the weather parameter which works better as a predictor
    
    while (z2<16){
      
      var1 <-data[[z1]]
      var2 <-data[[z2]]

      print(var1)
      print(var2)

      
      linear <- lm(formula=var2 ~ var1)
      ##summary(linear)
      RS <- cor(var2,var1,method="pearson") ## R2 Value
      ##print(RS)
      if (RS >= maiorrs) { ###Here we select which one of the six factors is better to work as predictors
        maiorrs=RS
        melhoropcao = z2
        
        ##print(z2) 
      }

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

    
    linearx <- lm(formula=data[[z1]] ~ data[[melhoropcao]])
    Cx <-coef(linearx)
    Bx <- Cx[[1]]
    Ax <- Cx[[2]]
    
    ax[1,z1]<-Ax
    bx[1,z1]<-Bx
    Dx[1,z1] <- melhoropcao
 
    ## A and B are the parameters AX + B = Y where the X is the chosen weather parameter and Y is the price
    ## Now we have to apply this to the test dataset for this specifi value of Z1 (type of product)
    melhoropcao <- 0
    z2 <- 10
    z1 <- z1+1
    
  }
  ## These are the predictions for each type from item 19 - Dessert to 27 - Salads
  quant19 <- (ax[1,19]*data1[Dx[1,19]])+bx[1,19]
  quant20 <- (ax[1,20]*data1[Dx[1,20]])+bx[1,20]
  quant21 <- (ax[1,21]*data1[Dx[1,21]])+bx[1,21]
  quant22 <- (ax[1,22]*data1[Dx[1,22]])+bx[1,22]
  quant23 <- (ax[1,23]*data1[Dx[1,23]])+bx[1,23]
  quant24 <- (ax[1,24]*data1[Dx[1,24]])+bx[1,24]
  quant25 <- (ax[1,25]*data1[Dx[1,25]])+bx[1,25]
  quant26 <- (ax[1,26]*data1[Dx[1,26]])+bx[1,26]
  quant27 <- (ax[1,27]*data1[Dx[1,27]])+bx[1,27]
