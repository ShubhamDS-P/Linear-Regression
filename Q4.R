#Q.3   Problem Statement :- Building a prediction model for salary hike based on the better R^2 value.
SH<-read.csv(file.choose())
View(SH)
attach(SH)
summary(SH)

# x = YearsExperience; y = Salary

X <- YearsExperience; Y <- Salary


plot(Y,X)

#Lets check the correlation between the given X and Y

cor(Y,X)    #correlation value =   0.9782416

#Lets create the model for predicting the Y value

mod<-lm(Y ~ X)
summary(mod)    # R-squared:  0.957

mod$coefficients   #gives Bo and B1

mod$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred<-predict(mod)   #store predicted values in the pred
pred

sqrt(mean(mod$residuals^2))         ##RMSE = 5592.044

confint(mod,level=0.95)
predict(mod,interval="predict")
pred1 <- predict(mod,interval="predict")
pred1

library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

###########################################################
###########################################################
# Logrithamic Model

# Model No.- 2 -  x = log(YearsExperience); y = Salary

X <- YearsExperience; Y <- Salary

plot(log(X), Y)
cor(log(X), Y)    #correlation value = 0.9240611

mod_log <- lm(Y ~ log(X))   # lm(Y ~ X)

summary(mod_log)     ## R-Squered:- 0.8539

pred<-predict(mod_log)    #store predicted values in the pred
pred

mod_log$residuals     #gives the error value of each record i.e. difference between pred and actual value

sqrt(mean(mod_log$residuals^2))         ##RMSE = 10302.89

confint(mod_log,level=0.95)
predict(mod_log,interval="confidence")
pred1 <- predict(mod_log,interval="confidence")
pred1

library(ggplot2)

ggplot(data = SH, aes(x = log(X), y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=log(X), y=pred))

####################################################################

# Model No.- 3 -  x = YearsExperience; y = log(Salary)


X <- YearsExperience; Y <- Salary


plot(X, log(Y))
cor(X, log(Y))     #correlation value :- 0.9653844

mod_log1 <- lm(log(Y) ~ X)   # lm(Y ~ X)

summary(mod_log1)    # R-Squered :- 0.932

tpred<-predict(mod_log1)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log1$residuals     #gives the error value of each record i.e. difference between pred and actual value

# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))    #RMSE value :-  7213.235
RMSE

confint(mod_log1,level=0.95)
predict(mod_log1,interval="confidence")

library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

#########################################################

# Model No.- 4 -  x = log(YearsExperience); y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(log(X), log(Y))
cor(log(X), log(Y))      #correlation value :- 0.9514279

mod_log2 <- lm(log(Y) ~ log(X))   # lm(Y ~ X)

summary(mod_log2)    # R_Squered :- 0.9052

tpred<-predict(mod_log2)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log2$residuals     #gives the error value of each record i.e. difference between pred and actual value



# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))   # RMSE value :- 7219.717
RMSE

confint(mod_log2,level=0.95)
predict(mod_log2,interval="confidence")

library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x= X, y= pred))

######################################################################
######################################################################
# Model No.- 5 -  Polynomial model with 2 degree (quadratic model)

# x = YearsExperience + (YearsExperience)^2 ; y = Salary

X <- YearsExperience; Y <- Salary


plot(X , Y)
plot(X*X , Y)

#Lets check the correlation between the given X and Y

cor(X*X , Y)    #correlation value = 0.9567235

#Lets create the model for predicting the Y value

mod_poly2 <- lm(Y ~ X + I(X*X))

summary(mod_poly2)    # R-squared:    0.957

mod_poly2$coefficients   #gives Bo and B1

mod_poly2$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly2$fitted.values   #store predicted values in the pred
pred

mod_poly2$fitted.values

sqrt(mean(mod_poly2$residuals^2))         ##RMSE = 5590.841   

confint(mod_poly2,level=0.95)
predict(mod_poly2,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

##################################################################

# Model No.- 6 -  Polynomial model with 2 degree with logarithmic Y (quadratic model)

# x = YearsExperience + (YearsExperience)^2 ; y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(X , log(Y))
plot(X*X , log(Y))

#Lets check the correlation between the given X and Y

cor(X*X , log(Y))    #correlation value =0.9157747

#Lets create the model for predicting the Y value

mod_poly2L <- lm(log(Y) ~ X + I(X*X))

summary(mod_poly2L)    # R-squared:  0.9486

mod_poly2L$coefficients   #gives Bo and B1

mod_poly2L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly2L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE =   5391.082

confint(mod_poly2L,level=0.95)
predict(mod_poly2L,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

#####################################################################
#####################################################################

# Model No.- 7 -  Polynomial model with 3 degree (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 ; y = Salary

X <- YearsExperience; Y <- Salary


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
#Lets check the correlation between the given X and Y

cor(X^3 , Y)    #correlation value = 0.9133658

#Lets create the model for predicting the Y value

mod_poly3 <- lm(Y ~ X + I(X*X) + I(X^3))

summary(mod_poly3)    # R-squared:  0.9636

mod_poly3$coefficients   #gives Bo and B1

mod_poly3$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly3$fitted.values   #store predicted values in the pred
pred

mod_poly3$fitted.values

sqrt(mean(mod_poly3$residuals^2))         ##RMSE =  5142.642

confint(mod_poly3,level=0.95)
predict(mod_poly3,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

##################################################################

# Model No.- 8 -  Polynomial model with 3 degree with logarithmic Y (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 ; y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^3 , log(Y))    #correlation value = 0.8565161

#Lets create the model for predicting the Y value

mod_poly3L <- lm(log(Y) ~ X + I(X*X) + I(X^3))

summary(mod_poly3L)    # R-squared:  0.992

mod_poly3L$coefficients   #gives Bo and B1

mod_poly3L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly3L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 5186.303

confint(mod_poly3L,level=0.95)
predict(mod_poly3L,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))
################################################################
################################################################
# Model No.- 9 -  Polynomial model with 4 degree (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4 ; y = Salary

X <- YearsExperience; Y <- Salary


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
#Lets check the correlation between the given X and Y

cor(X^4 , Y)    #correlation value = 0.8683542

#Lets create the model for predicting the Y value

mod_poly4 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4))

summary(mod_poly4)    # R-squared:  0.9637

mod_poly4$coefficients   #gives Bo and B1

mod_poly4$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly4$fitted.values   #store predicted values in the pred
pred

mod_poly4$fitted.values

sqrt(mean(mod_poly4$residuals^2))         ##RMSE = 5138.467

confint(mod_poly4,level=0.95)
predict(mod_poly4,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

################################################################
# Model No.- 10 -  Polynomial model with 4 degree with logarithmic Y (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4; y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^4 , log(Y))    #correlation value = 0.8029986

#Lets create the model for predicting the Y value

mod_poly4L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4))

summary(mod_poly4L)    # R-squared:   0.9516

mod_poly4L$coefficients   #gives Bo and B1

mod_poly4L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly4L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE =  5116.806

confint(mod_poly4L,level=0.95)
predict(mod_poly4L,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

################################################################
################################################################

# Model No.- 11 -  Polynomial model with 5 degree (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4 + (YearsExperience)^5 ; y = Salary

X <- YearsExperience; Y <- Salary


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
#Lets check the correlation between the given X and Y

cor(X^5 , Y)    #correlation value = 0.8266089

#Lets create the model for predicting the Y value

mod_poly5 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5))

summary(mod_poly5)    # R-squared:  0.9666

mod_poly5$coefficients   #gives Bo and B1

mod_poly5$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly5$fitted.values   #store predicted values in the pred
pred

mod_poly5$fitted.values

sqrt(mean(mod_poly5$residuals^2))         ##RMSE = 4929.3

confint(mod_poly5,level=0.95)
predict(mod_poly5,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

##########################################################################

# Model No.- 12 -  Polynomial model with 5 degree with logarithmic Y (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4 + (YearsExperience)^5; y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
plot(X^5 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^5 , log(Y))    #correlation value =0.7568183

#Lets create the model for predicting the Y value

mod_poly5L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5))

summary(mod_poly5L)    # R-squared:   0.9529

mod_poly5L$coefficients   #gives Bo and B1

mod_poly5L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly5L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 4909.462

confint(mod_poly5L,level=0.95)
predict(mod_poly5L,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

####################################################################
####################################################################

# Model No.- 13 - Polynomial model with 6 degree (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4 + (YearsExperience)^5 + (YearsExperience)^6 ; y = Salary

X <- YearsExperience; Y <- Salary


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
plot(X^6 , Y)
#Lets check the correlation between the given X and Y

cor(X^6 , Y)    #correlation value = 0.7889879

#Lets create the model for predicting the Y value

mod_poly6 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5) + I(X^6))

summary(mod_poly6)    # R-squared:    0.9674

mod_poly6$coefficients   #gives Bo and B1

mod_poly6$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly6$fitted.values   #store predicted values in the pred
pred

mod_poly6$fitted.values

sqrt(mean(mod_poly6$residuals^2))         ##RMSE = 4868.895

confint(mod_poly6,level=0.95)
predict(mod_poly6,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))


######################################################################

# Model No.- 14 -  Polynomial model with 6 degree with logarithmic Y (quadratic model)

# x = YearsExperience + (YearsExperience)^2 + (YearsExperience)^3 + (YearsExperience)^4 + (YearsExperience)^5 + (YearsExperience)^6; y = log(Salary)

X <- YearsExperience; Y <- Salary


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
plot(X^5 , log(Y))
plot(X^6 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^6 , log(Y))    #correlation value = 0.7170135


#Lets create the model for predicting the Y value

mod_poly6L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5) + I(X^6))

summary(mod_poly6L)    # R-squared:    0.9545

mod_poly6L$coefficients   #gives Bo and B1

mod_poly6L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly6L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 5053.83                 

confint(mod_poly6L,level=0.95)
predict(mod_poly6L,interval="predict")


library(ggplot2)

ggplot(data = SH, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = SH, aes(x=X, y=pred))

######################################################################
####################################################################

# Answer:- Above are the models which we have created for predicting the salary hike using the experience of the various employees.
# From the data shown in the Q4 Excel datasheet we can conclude that the model no. 13 is the best model with which we have got the best results and the highest R-Squared value so far
# Hence we are going to use the model no. 13 for the required predictions.