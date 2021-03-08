setwd("/Curso-ML/Assignment-1/")
data0 <- read.csv("data-06.csv") 
##varyx <-seq(from media=-1, 1, by =.1)
varyy <- data0[[19]]
print(varyy)
print(varyx)

media <- mean(varyy, trim = 0, na.rm = TRUE)
print(media)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(varyx, mean = media, sd = 30)
print(y)

m <- media # mean
s <- 30 # standard deviation
x <- seq(from=m-5*s, to=m+5*s,by=1) ## X values to be plotted
y <- dnorm(x,mean=m,sd=s) ## Normal density
plot(x, y,type="l",col="darkgreen",lwd=2, main="Standard normal density function",las=1)
legend(min(x),max(y), legend=paste("mean=",m,"; sd=",s,sep=""))
grid(col="black")
abline(v=m,col="purple", lwd=2)
abline(v=m-s,col="lightblue", lwd=2)
abline(v=m+s,col="lightblue", lwd=2)