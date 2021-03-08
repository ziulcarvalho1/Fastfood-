setwd("/Curso-ML/Assignment-1/")
variables <- read.csv("data-06.csv")
summary(variables)
variables$diasemana <- NULL
variables1 <- variables[10:31]
summary(variables1)
library(corrplot)
cor.test(variables1$diasem, variables1$pizzas)
cor.test(variables1$diasem, variables1$pizzas)
forcorrplot <-cor(variables1)
corrplot(forcorrplot)
corrplot(forcorrplot, method = "square")
corrplot(forcorrplot, upper="number", lower="color", order="hclust")
corrplot(forcorrplot, method="number",order="hclust" )
