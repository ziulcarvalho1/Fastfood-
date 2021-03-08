library(shiny)
library(dplyr)
setwd("/Curso-ML/Assignment-1/")
shinyUI(fluidPage(
  
  titlePanel(title="Sales Forecast"),
  sidebarLayout(position ="right",
    sidebarPanel(("Enter the parameters selection"),
                 selectInput("mes","Select the month of the year",c("January", "February", "March","April","May","June","July","August","September","October","November","December"),selected="January",selectize = FALSE),
                 radioButtons("day","Select the day of the week", list("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"), "Monday"),
                 sliderInput("tempmin", "Select the minimum temperature",min=0,max=30,value=15),
                 sliderInput("tempmed", "Select the average temperature",min=0,max=40,value=20),
                 sliderInput("tempmax", "Select the maximum temperature",min=0,max=50,value=25),
                 sliderInput("precip" , "Select level of rain in mm "   ,min=0,max=100,value=20),
                 sliderInput("sun"    , "Select the sun exposition "    ,min=0,max=20,value=10, step=0.5),
                 sliderInput("humid"  , "Select air humidity %"         ,min=0,max=100,value=50),
                 submitButton("Update")

                 ),
   
     mainPanel(
      textOutput("mymes"),
      textOutput("myday"),
      textOutput("mytempmin"),
      textOutput("mytempmed"),
      textOutput("mytempmax"),
      textOutput("myprecip"),
      textOutput("mysun"),
      textOutput("myhumid"),
      textOutput("prediction1")
     
      
    )
  )
    
  )
)
  

