#Q.1   Problem Statement :-  Predict weight gained using calories consumed.
cc<-read.csv(file.choose())
View(cc)
attach(cc)
summary(cc)

# x = Calories.Consumed; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(Y,X)

#Lets check the correlation between the given X and Y
cor(Y,X)    #correlation value = 0.946991

#Lets create the model for predicting the Y value

mod<-lm(Y ~ X)
summary(mod)    # R-squared:  0.8968

mod$coefficients   #gives Bo and B1

mod$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred<-predict(mod)   #store predicted values in the pred
pred

sqrt(mean(mod$residuals^2))         ##RMSE = 103.3025

confint(mod,level=0.95)
predict(mod,interval="predict")
pred1 <- predict(mod,interval="predict")
pred1

library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

###########################################################
###########################################################
# Logrithamic Model

# x = log(Calories.Consumed); y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.

plot(log(X), Y)
cor(log(X), Y)

mod_log <- lm(Y ~ log(X))   # lm(Y ~ X)

summary(mod_log)
pred<-predict(mod_log)    #store predicted values in the pred
pred

mod_log$residuals     #gives the error value of each record i.e. difference between pred and actual value
sqrt(mean(mod_log$residuals^2))         ##RMSE = 141.0054

confint(mod_log,level=0.95)
predict(mod_log,interval="confidence")
pred1 <- predict(mod_log,interval="confidence")
pred1

library(ggplot2)

ggplot(data = cc, aes(x = log(X), y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=log(X), y=pred))

####################################################################

# x = Calories.Consumed; y = log(Weight.gained..grams.)


X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X, log(Y))
cor(X, log(Y))

mod_log1 <- lm(log(Y) ~ X)   # lm(Y ~ X)

summary(mod_log1)
tpred<-predict(mod_log1)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log1$residuals     #gives the error value of each record i.e. difference between pred and actual value

# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))
RMSE

confint(mod_log1,level=0.95)
predict(mod_log1,interval="confidence")

library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

  ##########################################

# x = log(Calories.Consumed); y = log(Weight.gained..grams.)

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(log(X), log(Y))
cor(log(X), log(Y))

mod_log2 <- lm(log(Y) ~ log(X))   # lm(Y ~ X)

summary(mod_log2)
tpred<-predict(mod_log2)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log2$residuals     #gives the error value of each record i.e. difference between pred and actual value



# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))
RMSE

confint(mod_log2,level=0.95)
predict(mod_log2,interval="confidence")

library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x= X, y= pred))

######################################################################
##############################################################
##################################################
################################
########################
##################
#######
# Polynomial model with 2 degree (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 ; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , Y)
plot(X*X , Y)

#Lets check the correlation between the given X and Y

cor(X*X , Y)    #correlation value = 0.9710636

#Lets create the model for predicting the Y value

mod_poly2 <- lm(Y ~ X + I(X*X))

summary(mod_poly2)    # R-squared:  0.9521

mod_poly2$coefficients   #gives Bo and B1

mod_poly2$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly2$fitted.values   #store predicted values in the pred
pred

mod_poly2$fitted.values

sqrt(mean(mod_poly2$residuals^2))         ##RMSE = 70.40752

confint(mod_poly2,level=0.95)
predict(mod_poly2,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

##################################################################

# Polynomial model with 2 degree with logarithmic Y (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 ; y = log(Weight.gained..grams.)

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , log(Y))
plot(X*X , log(Y))

#Lets check the correlation between the given X and Y

cor(X*X , log(Y))    #correlation value = 0.9267624

#Lets create the model for predicting the Y value

mod_poly2L <- lm(log(Y) ~ X + I(X*X))

summary(mod_poly2L)    # R-squared:  0.8776

mod_poly2L$coefficients   #gives Bo and B1

mod_poly2L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly2L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 117.4145

confint(mod_poly2L,level=0.95)
predict(mod_poly2,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

#####################################################################
#####################################################################

# Polynomial model with 3 degree (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 + (Calories.Consumed)^3 ; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
#Lets check the correlation between the given X and Y

cor(X^3 , Y)    #correlation value = 0.971167

#Lets create the model for predicting the Y value

mod_poly3 <- lm(Y ~ X + I(X*X) + I(X^3))

summary(mod_poly3)    # R-squared:  0.9811

mod_poly3$coefficients   #gives Bo and B1

mod_poly3$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly3$fitted.values   #store predicted values in the pred
pred

mod_poly3$fitted.values

sqrt(mean(mod_poly3$residuals^2))         ##RMSE = 44.1501

confint(mod_poly3,level=0.95)
predict(mod_poly3,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

##################################################################

# Polynomial model with 3 degree with logarithmic Y (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 + (Calories.Consumed)^3 ; y = log(Weight.gained..grams.)

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^3 , log(Y))    #correlation value = 0.9267624

#Lets create the model for predicting the Y value

mod_poly3L <- lm(log(Y) ~ X + I(X*X) + I(X^3))

summary(mod_poly3L)    # R-squared:  0.9417

mod_poly3L$coefficients   #gives Bo and B1

mod_poly3L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly3L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 73.79379

confint(mod_poly3L,level=0.95)
predict(mod_poly3,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))
################################################################
################################################################
# Polynomial model with 4 degree (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 + (Calories.Consumed)^3 + (Calories.Consumed)^4 ; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
#Lets check the correlation between the given X and Y

cor(X^4 , Y)    #correlation value = 0.9534202

#Lets create the model for predicting the Y value

mod_poly4 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4))

summary(mod_poly4)    # R-squared:  0.983

mod_poly4$coefficients   #gives Bo and B1

mod_poly4$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly3$fitted.values   #store predicted values in the pred
pred

mod_poly4$fitted.values

sqrt(mean(mod_poly4$residuals^2))         ##RMSE = 41.97848

confint(mod_poly4,level=0.95)
predict(mod_poly4,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))

################################################################
################################################################
# Polynomial model with 5 degree (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 + (Calories.Consumed)^3 + (Calories.Consumed)^4 + (Calories.Consumed)^5 ; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
#Lets check the correlation between the given X and Y

cor(X^5 , Y)    #correlation value = 0.9256327

#Lets create the model for predicting the Y value

mod_poly5 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5))

summary(mod_poly5)    # R-squared:  0.9843

mod_poly5$coefficients   #gives Bo and B1

mod_poly5$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly5$fitted.values   #store predicted values in the pred
pred

mod_poly5$fitted.values

sqrt(mean(mod_poly5$residuals^2))         ##RMSE = 40.29204

confint(mod_poly5,level=0.95)
predict(mod_poly5,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))
####################################################################
####################################################################
# Polynomial model with 6 degree (quadratic model)

# x = Calories.Consumed + (Calories.Consumed)^2 + (Calories.Consumed)^3 + (Calories.Consumed)^4 + (Calories.Consumed)^5 + (Calories.Consumed)^6 ; y = Weight.gained..grams.

X <- Calories.Consumed; Y <- Weight.gained..grams.


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
plot(X^6 , Y)
#Lets check the correlation between the given X and Y

cor(X^6 , Y)    #correlation value = 0.8941256

#Lets create the model for predicting the Y value

mod_poly6 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5) + I(X^6))

summary(mod_poly6)    # R-squared:   0.9867

mod_poly6$coefficients   #gives Bo and B1

mod_poly6$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly6$fitted.values   #store predicted values in the pred
pred

mod_poly6$fitted.values

sqrt(mean(mod_poly6$residuals^2))         ##RMSE = 37.14769

confint(mod_poly6,level=0.95)
predict(mod_poly6,interval="predict")


library(ggplot2)

ggplot(data = cc, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cc, aes(x=X, y=pred))
