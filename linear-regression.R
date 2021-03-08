library(ranger)
library(caret)
library(data.table)
library(caTools)

setwd("/Curso-ML/Assignment-1/")
input_variables <- read.csv("data-06.csv")
data_sample = sample.split(input_variables,SplitRatio=0.80)
input_variables_values_train_datasets = subset(input_variables,data_sample==TRUE)
input_variables_values_test_datasets  = subset(input_variables,data_sample==FALSE)
dim(input_variables_values_train_datasets)
dim(input_variables_values_test_datasets)
summary(input_variables_values_train_datasets)
summary(input_variables_values_test_datasets)
head(input_variables_values_test_datasets)
x_train <- input_variables_values_test_datasets$diasem
y_train <- input_variables_values_test_datasets$pizzas
x_test <- input_variables_values_test_datasets
x <- cbind(x_train,y_train)
class(x_train)

scatter.smooth(x=input_variables_values_test_datasets$diasem, y=input_variables_values_test_datasets$pizzas, main="Dias ~ Pizzas")  # scatterplot

class(x_train)
print(x_train)

print(y_train)
print(x)
# Train the model using the training sets and check score
###linear <- lm(y_train ~ ., data = x)
linear <- lm(formula=x_train ~ y_train, data=input_variables_values_test_datasets)
summary(linear)
#Predict Output
predicted= predict(linear,x_test)
2. Logistic Regression
Don’t get confused by its name! It is a classification not a regression algorithm. It is used to
estimate discrete values ( Binary values like 0/1, yes/no, true/false ) based on given set of
independent variable(s). In simple words, it predicts the probability of occurrence of an event
by fitting data to a logit function. Hence, it is also known as logit regression. Since, it predicts
the probability, its output values lies between 0 and 1 (as expected).
Again, let us try and understand this through a simple example.
Let’s say your friend gives you a puzzle to solve. There are only 2 outcome scenarios – either
you solve it or you don’t. Now imagine, that you are being given wide range of puzzles / quizzes
in an attempt to understand which subjects you are good at. The outcome to this study would
be something like this – if you are given a trignometry based tenth grade problem, you are 70%
likely to solve it. On the other hand, if it is grade fifth history question, the probability of getting
an answer is only 30%. This is what Logistic Regression provides you.
Coming to the math, the log odds of the outcome is modeled as a linear combination of the
predictor variables.
odds= p/ (1-p) = probability of event occurrence / probability of not event occurrence