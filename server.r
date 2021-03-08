library(shiny)
library(dplyr)
library(tidyr)

shinyServer(
  function(input,output){
  
    output$mymes     <- renderText(paste("You selected the month of the year   as: ",input$mes))
    output$myday     <- renderText(paste("You selected the day of the week     as: ",input$day))
    ##output$mytempmin <- renderText(paste("You selected the minimum temperature as: ",input$tempmin,"C"))
    output$mytempmed <- renderText(paste("You selected the medium  temperature as: ",input$tempmed,"C"))
    ##output$mytempmax <- renderText(paste("You selected the maximum temperature as: ",input$tempmax,"C"))
    output$myprecip  <- renderText(paste("You selected the level of rain       as: ",input$precip,"mm"))
    output$mysun     <- renderText(paste("You selected the level of sun exposition as: ",input$sun,"UV"))
    output$myhumid   <- renderText(paste("You selected the level of humidity in the air as: ",input$humid,"%"))
 
    
    # Reactive expression to create data frame of all input values ----
    inputData <- reactive({
      
        ###name = c("precip",  "tempmax", "tempmin", "tempmed", "humid","sun", "mes", "day"),
        
        value = (c(input$precip, input$tempmed, input$humid, input$sun, input$day, input$mes))
      
    })
    
    # Use model on input data
    
    df_5 <- reactive({
      data <- (inputData())
      data10    <- data[1]
      data11    <- "25"
      data12    <- "15"
      data13    <- data[2]
      data14    <- data[3]
      data15    <- data[4]
      data16    <- data[5]
      data17    <- data[6]
      
      data10x <- as.numeric(data10)
      data11x <- as.numeric(data11)
      data12x <- as.numeric(data12)
      data13x <- as.numeric(data13)
      data14x <- as.numeric(data14)
      data15x <- as.numeric(data15)
      
      if (data17 == "January"){
        data17x <- 3
      }
      else{
        if (data17 == "February"){
         data17x <- 1
        }
        else{
          if (data17 == "March"){
            data17x <- 2
          }
          else{
            if(data17 == "April"){
              data17x <- 2
            }
            else{
              if (data17 == "May"){
                data17x <- 2
              }
              else{
                if (data17 == "June"){
                  data17x <- 3
                }
                else{
                  if (data17 == "July"){
                    data17x <- 1
                  }
                  else{
                    if (data17 == "August"){
                      data17x <- 2
                    }
                    else{
                      if (data17 == "September"){
                        data17x <- 2
                      }
                      else{
                        if (data17 == "October"){
                          data17x <-2
                        }
                        else{
                          if (data17 == "November"){
                            data17x <- 2
                          }
                          else{
                            data17x <-3
                          }
                        }
                      }
                    }
                  }
                }
               }
            }
          }
        }
      }
      if (data16 == "Friday"|data16 == "Saturday"){
        data16x <- 3
      }
      else{
        if (data16 == "Sunday"){
          data16x <- 2
        }
        else{
         data16x <- 1
        }
      }
      
      #######################################
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
      ex<-matrix(, nrow=1, ncol=27)
      qx<-matrix(, nrow=9, ncol=2)
      data1 <- matrix(,nrow=1,ncol=27)
      
      for (ss1 in 1:27){
        ax[1,ss1]<-0
        bx[1,ss1]<-0 
        Dx[1,ss1]<-0
        data1[1,ss1]<-0
      }
      
      class(ax)
      ## these are the values gotten from the shimy app
      dia  <- data16x
      mes1 <- data17x
      data1[10]    <-  data10x ## da
      data1[11]    <-  data11x ##30
      data1[12]    <-  data12x ##15
      data1[13]    <-  data13x ##20
      data1[14]    <-  data14x ##70
      data1[15]    <-  data15x ##2.4
      
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
          
          ##print(var1)
          ##print(var2)
          
          
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
        
        Ex <- confint(linearx, "data[[melhoropcao]]", level = 0.95)
        Ex1 <-Ex[[2]]
        
        ex[1,z1]<-(2*Ex1)
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
      
      quant19 <- round(quant19, digits = 0)
      quant20 <- round(quant20, digits = 0)
      quant21 <- round(quant21, digits = 0)
      quant22 <- round(quant22, digits = 0)
      quant23 <- round(quant23, digits = 0)
      quant24 <- round(quant24, digits = 0)
      quant25 <- round(quant25, digits = 0)
      quant26 <- round(quant26, digits = 0)
      quant27 <- round(quant27, digits = 0)
      
      qx[1,2] <- (quant19)
      qx[2,2] <- (quant20)
      qx[3,2] <- (quant21)
      qx[4,2] <- (quant22)
      qx[5,2] <- (quant23)
      qx[6,2] <- (quant24)
      qx[7,2] <- (quant25)
      qx[8,2] <- (quant26)
      qx[9,2] <- (quant27)
      
      qx[1,1] <- "Desserts"
      qx[2,1] <- "Pizzas"
      qx[3,1] <- "Beverage"
      qx[4,1] <- "Sfihas"
      qx[5,1] <- "Snack"
      qx[6,1] <- "Pastas"
      qx[7,1] <- "Dishes"
      qx[8,1] <- "Savory"
      qx[9,1] <- "Salads"
      
      ##print(qx[,])
      ##value2 <- c(quant19," - Desserts - ",quant20," - Pizzas - ",quant21," - Beverage - ",quant22," - Sfiha - ",quant23," - Snack - ", quant24," - Pastas - ", quant25," - Dishes - ",quant26," - Savory - ",quant27," - Salads -")
      ##dataxxx <- data.frame(quant27)
      name = c("Desserts",  "Pizzas", "Beverage", "Sfihas", "Snack","Pastas", "Dishes", "Savory","Salads")
      library(data.table)
      library(DT)
      value1 = c(quant19,quant20,quant21,quant22,quant23,quant24,quant25,quant26,quant27)
      df_0 <- data.frame(Item = c("Desserts",  "Pizzas", "Beverage", "Sfihas", "Snack","Pastas", "Dishes", "Savory","Salads"),
                         Quant = c(quant19,quant20,quant21,quant22,quant23,quant24,quant25,quant26,quant27), 
                         Standarddev = c(ex[1,19],ex[1,20],ex[1,21],ex[1,22],ex[1,23],ex[1,24],ex[1,25],ex[1,26],ex[1,27]),
                         Standarddevper = c((100*ex[1,19]/quant19),(100*ex[1,20]/quant20),(100*ex[1,21]/quant21),(100*ex[1,22]/quant22),(100*ex[1,23]/quant23),(100*ex[1,24]/quant24),(100*ex[1,25]/quant25),(100*ex[1,26]/quant26),(100*ex[1,27]/quant27)),
                         lower = c((quant19-ex[1,19]),(quant20-ex[1,20]),(quant21-ex[1,21]),(quant22-ex[1,22]),(quant23-ex[1,23]),(quant24-ex[1,24]),(quant25-ex[1,25]),(quant26-ex[1,26]),(quant27-ex[1,27])),
                         upper = c((quant19+ex[1,19]),(quant20+ex[1,20]),(quant21+ex[1,21]),(quant22+ex[1,22]),(quant23+ex[1,23]),(quant24+ex[1,24]),(quant25+ex[1,25]),(quant26+ex[1,26]),(quant27+ex[1,27]))
      )
      data1x <-df_0
      
      write.csv(data1x,"/Curso-ML/Assignment-1/result-06.csv")
      
      df_1 <- data.frame(Item = c("Desserts",  "Pizzas", "Beverage", "Sfihas", "Snack","Pastas", "Dishes", "Savory","Salads"),
                         ##Quant = c(quant19,quant20,quant21,quant22,quant23,quant24,quant25,quant26,quant27), 
                         Standarddev = c(ex[1,19],ex[1,20],ex[1,21],ex[1,22],ex[1,23],ex[1,24],ex[1,25],ex[1,26],ex[1,27]),
                         Standarddevper = c((100*ex[1,19]/quant19),(100*ex[1,20]/quant20),(100*ex[1,21]/quant21),(100*ex[1,22]/quant22),(100*ex[1,23]/quant23),(100*ex[1,24]/quant24),(100*ex[1,25]/quant25),(100*ex[1,26]/quant26),(100*ex[1,27]/quant27)),
                         lower = c((quant19-ex[1,19]),(quant20-ex[1,20]),(quant21-ex[1,21]),(quant22-ex[1,22]),(quant23-ex[1,23]),(quant24-ex[1,24]),(quant25-ex[1,25]),(quant26-ex[1,26]),(quant27-ex[1,27])),
                         upper = c((quant19+ex[1,19]),(quant20+ex[1,20]),(quant21+ex[1,21]),(quant22+ex[1,22]),(quant23+ex[1,23]),(quant24+ex[1,24]),(quant25+ex[1,25]),(quant26+ex[1,26]),(quant27+ex[1,27]))
                         )
    
       
      ##df_2 <- as.data.table(df_1, keep.rownames=TRUE)
      ##df_2 <- (qx[,])
      ##df_2 <- as.data.frame(qx[,])
      ##df_2 <- as.table(qx[,], keep.rownames=TRUE)
      ##df_2 <- as.table(df_1, keep.rownames=TRUE)
      ##df_3 <- renderTable(df_2)
      ##df_2 <- value2
      ##df_4 <- renderText({value2})
      ##value2 <- paste(quant19," - Desserts - ",quant20," - Pizzas - ",quant21," - Beverage - ",quant22," - Sfiha - ",quant23," - Snack - ", quant24," - Pastas - ", quant25," - Dishes - ",quant26," - Savory - ",quant27," - Salads -")
      ##df_2 <- paste(df_1[1],df_1[2])
      ##df_2 <- as.matrix(df_1,rownames=TRUE )
      ##df_2 <- as.matrix(qx[,])
      print(df_0)
      print(df_1)
      ## print(data.frame(df_2))
      ## df_1 <- spread(df_1, name, value1)
      #library(data.table)
      #library(dplyr)
      ##df_2 <- data.frame(df_1[1],df_1[2])
      ##print(df_1)
      ##output$prediction1 <- renderDataTable(df_2)
      ##output$prediction1 <- df_2
      ##df_3 <- DT::renderDataTable({df_1}) 
      ##dim(df_2)
      
      ##output$prediction1 <- DT::renderDataTable({df_1})
      
    
      
      ##print(df_2, class = TRUE)
      ## qx[1,] <- name
      ## qx[2,] <- value1
      ##df_2 <- renderText(paste("Desserts: ",quant19,"Pizzas  : ",quant20))
      ##df_1 <- data.table(df_1)
      ##dim(df_1)
      ##df_1 <- c(df_1)
      ##print(dataxxx)
      ####################data <- as.integer(data)
      ##outcome <- predict(model, data = data)
      
    }) ## final of output$predicition1
    
    
    #print(class(df_5))
    ##DT::renderDataTable({df_5}) ##

    #output$prediction1 <- df_5 #DT::renderDataTable({df_5}) ## 
    data2x <- read.csv("/Curso-ML/Assignment-1/result-06.csv") 
    print(data2x)
    print(df_5)
    library(ggplot2)
    
    
    
    output$view   <- renderTable({df_5()})
    output$hist <-renderPlot({
      ggplot(df_5(),x= Item, y=lower, aes(Item, y = round(lower, digits = 0), fill=Item)) + 
        geom_bar(stat="identity", position = 'dodge') +
        geom_text(aes(label=round(lower, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25)
      })
    
     
 }

  
 
  
)

