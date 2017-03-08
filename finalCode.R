# Filename: finalCode
#Authors: Himanshu Makhija, Hanna Goldman, Amey Paranjape, Shagun Gupta
#Class: MATH 189 -- Winter, 2017
#Date: 3/7/2017

data <- read.table("gauge.txt", header=TRUE)
gain <- data$gain
density <- data$density
plot(x=gain, y=density)
# Least Squares Fit
fit <- lm(density~gain, data)
fit
abline(fit,col="red")
plot(fit$residuals)
abline(0, 0, col="red")
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")
# Quadratic Fit
fit.2 <- lm(density~gain+I(gain^2),data)
fit.2
# Log Transform
logG <- log(gain)
fit.3 <- lm(density~logG, data)
fit.3
plot(fit.3$residuals)
abline(0, 0, col="red")
plot(logG,density)
abline(fit.3,col="red")
qqnorm(fit.3$residuals)
qqline(fit.3$residuals, col="red")
#R^2 Check
summary(fit)
summary(fit.2)
summary(fit.3)
#Quantile Regression
quantile_loss <- function(x, tau) {
  ind <- 1*(x < 0)
  return (x*(tau - ind))
}
x<- 1.2980 - 0.2162*(logG)
plot(x, quantile_loss(x, 0.2), type = "l", lty = 1, col = 1, ylab = "quantile loss")
lines(x, quantile_loss(x, 0.4), lty = 2, col = 2)
lines(x, quantile_loss(x, 0.5), lty = 3, col = 3, lwd = 5)
lines(x, quantile_loss(x, 0.6), lty = 4, col = 4)
lines(x, quantile_loss(x, 0.8), lty = 5, col = 5)
legend(x = 0.6, y = 0.10, legend = c(expression(paste(rho, "=", 0.2)),
                                expression(paste(rho, "=", 0.4)),
                                expression(paste(rho, "=", 0.5)),
                                expression(paste(rho, "=", 0.6)),
                                expression(paste(rho, "=", 0.8))),
       lty = c(1,2,3,4,5), lwd = c(1,1,5,1,1), col = c(1,2,3,4,5), cex=0.5)
quantile(x, probs = seq(0.1, 0.9, by = 0.2))

#Confidence Interval
qt(.95, df=88)

#Removing Outliers
#part3
points = data[data$density==0.508,]
dataout1 <- data[data$density != 0.508,]
dataout2 <- data[data$density != 0.001,]
pointx = data[data$density == 0.001,]
#part 3.1
gain <- dataout1$gain
density <- dataout1$density
plot(x=gain, y=density)
# Least Squares Fit
fito1 <- lm(density~gain, dataout1)
fito1
abline(fito1,col="red")
plot(fit$residuals)
abline(0, 0, col="red")
hist(fito1$residuals)
qqnorm(fito1$residuals)
qqline(fito1$residuals, col="red")
# Quadratic Fit
fito1.2 <- lm(density~gain+I(gain^2),dataout1)
fito1.2
# Log Transform
logG <- log(gain)
fito1.3 <- lm(density~logG, dataout1)
fito1.3
plot(fito1.3$residuals)
abline(0, 0, col="red")
plot(logG,density)
abline(fito1.3,col="red")
qqnorm(fito1.3$residuals)
qqline(fito1.3$residuals, col="red")


summary(fito1)


summary(fito1.2)


#Quantile Regression
quantile_loss <- function(x, tau) {
  ind <- 1*(x < 0)
  return (x*(tau - ind))
}
x<- 1.2980 - 0.2162*(logG)
plot(x, quantile_loss(x, 0.2), type = "l", lty = 1, col = 1, ylab = "quantile loss")
lines(x, quantile_loss(x, 0.4), lty = 2, col = 2)
lines(x, quantile_loss(x, 0.5), lty = 3, col = 3, lwd = 5)
lines(x, quantile_loss(x, 0.6), lty = 4, col = 4)
lines(x, quantile_loss(x, 0.8)í ½, lty = 5, col = 5)
      
      
      #part 3.2
      gain <- dataout2$gain
      density <- dataout2$density
      plot(x=gain, y=density)
      # Least Squares Fit
      fito2 <- lm(density~gain, dataout2)
      fito2
      abline(fito2,col="red")
      plot(fito2$residuals)
      abline(0, 0, col="red")
      hist(fito2$residuals)
      qqnorm(fito2$residuals)
      qqline(fito2$residuals, col="red")
      # Quadratic Fit
      fito2.2 <- lm(density~gain+I(gain^2),dataout1)
      fito2.2
      # Log Transform
      logG <- log(gain)
      fito2.3 <- lm(density~logG, dataout1)
      fito2.3
      plot(fito2.3$residuals)
      abline(0, 0, col="red")
      plot(logG,density)
      abline(fito2.3,col="red")
      qqnorm(fito2.3$residuals)
      qqline(fito2.3$residuals, col="red")
      
      
      summary(fito2)
      
      
      summary(fito2.2)
      
      
      #Quantile Regression
      quantile_loss <- function(x, tau) {
        ind <- 1*(x < 0)
        return (x*(tau - ind))
      }
      x<- 1.2980 - 0.2162*(logG)
      plot(x, quantile_loss(x, 0.2), type = "l", lty = 1, col = 1, ylab = "quantile loss")
      lines(x, quantile_loss(x, 0.4), lty = 2, col = 2)
      lines(x, quantile_loss(x, 0.5), lty = 3, col = 3, lwd = 5)
      lines(x, quantile_loss(x, 0.6), lty = 4, col = 4)
      lines(x, quantile_loss(x, 0.8), lty = 5, col = 5)

####Read dataset again from txt file
dat <- read.table("gauge.txt",header = T)
summary(dat)

# Loading dataset
gauge <- read.csv("gauge.txt", sep="")
head(gauge)
density <- gauge$density
gain <- gauge$gain
Y<-cbind(density)
X<-cbind(gain)
corr<-cor(Y,X)
# 9 Unique values of density for which observations have been recorded
obs.density <- unique(gauge$density)
# gain observations for each unique density stored as a row vector
obs.gain <- matrix(nrow = length(obs.density),ncol = nrow(gauge)/length(obs.density))
for (i in 1:length(obs.density)){
  obs.gain[i,] <- gauge$gain[which(gauge$density==unique(gauge$density)[i])]
}
# summary statistics for gain observations of each unique density
obs.mean <- numeric(length(obs.density))
obs.sd <- numeric(length(obs.density))
for (i in 1:length(obs.density)){
  obs.mean[i] <- mean(obs.gain[i,])
  obs.sd[i] <- sd(obs.gain[i,])
}
print(matrix(c(obs.density,obs.mean,obs.sd),nrow = 9,ncol=3,dimnames = list(1:9,c("Density","Mean(gain)"," Std.dev(gain)"))))

str(gauge)
library(ggplot2)

ggplot(gauge,aes(x = density, y=gain))+geom_point(col="blue",size=2)+labs(y="Gain",x="Density",title="Scatter plot of Observed Data (90 observations)")+geom_smooth(mapping = aes(x = density, y=gain),col='black',se=FALSE)+geom_smooth(method = "lm", se = FALSE,col='red')


##checking whether coeff is significant or not
model1=lm(density~gain,data=gauge)
plot(model1)
summary(model1)
confint(model1, level=0.95)

#scatterplot of log-transformed data
ggplot(gauge,aes(x = density, y=log(gain)))+geom_point(col="blue",size=2)+labs(y="Log(Gain)",x="Density",title="Scatter plot of Log-Transformed Observed Data (90 observations)")+geom_smooth(mapping = aes(x = density, y=log(gain)),col='black',se= FALSE)


ggplot(gauge,aes(x = log(density), y=gain))+geom_point(col="blue",size=2)+labs(y="Gain",x="Log(Density)",title="Scatter plot of Log-Transformed Observed Data (90 observations)")+geom_smooth(mapping = aes(x = log(density), y=gain),col='black',se= FALSE)

#linear regression for log-linear transformed data
ggplot(gauge,aes(x = density, y=log(gain)))+geom_point(col="blue",size=2)+labs(y="Log(Gain)",x="Density",title="Scatter plot of Log-Transformed Observed Data (90 observations)")+geom_smooth(method = "lm", se = FALSE,col='red')+geom_smooth(mapping = aes(x = density, y=log(gain)),col='black',se= FALSE)

#residuals for log-linear transformed data & Goodness of fit
model2=lm(density~log(gain))
plot(model2)
##checking whether coeff is significant or not
summary(model2)
confint(model2, level=0.95)

#correlation of log-linear model
fit.log <- lm(density~I(log(gain)) )
fit.linear <- lm( density~I(gain) )
install.packages("ellipse")
library(ellipse)
plotcorr(cor(gauge))
print("The correlation matrix of the given data is: ")
cor(gauge)


#residuals
res.linear <- as.numeric(residuals(fit.linear))
res.log <- as.numeric(residuals(fit.log))

# To check if residuals are nearly normal
a<-fortify(fit.linear)
sres.linear <- as.numeric(a$.stdresid)
#linear regression
ggplot(b, aes(sres.linear))+geom_histogram(binwidth = diff(range(sres.linear))/8)+labs(x="Standardized Residuals of linear Fit Model",y="Counts",title="Histogram of Residuals of linear Fit Model")+geom_smooth(aes(y=45*dnorm(sres.linear,mean=mean(sres.linear),sd=sd(sres.linear))),se = FALSE)


#log-linear regression
b<-fortify(fit.log)
sres.log <- as.numeric(b$.stdresid)
ggplot(b, aes(sres.log))+geom_histogram(binwidth = diff(range(sres.log))/8)+labs(x="Standardized Residuals of Log-linear Fit Model",y="Counts",title="Histogram of Residuals of Log-linear Fit Model")+geom_smooth(aes(y=45*dnorm(sres.log,mean=mean(sres.log),sd=sd(sres.log))),se = FALSE)

