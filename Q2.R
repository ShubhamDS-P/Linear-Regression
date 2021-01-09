#Q.2   Problem Statement :- Predict delivery time using sorting time.
DT<-read.csv(file.choose())
View(DT)
attach(DT)
summary(DT)

# x = Sorting.Time; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(Y,X)

#Lets check the correlation between the given X and Y
cor(Y,X)    #correlation value =  0.8259973

#Lets create the model for predicting the Y value

mod<-lm(Y ~ X)
summary(mod)    # R-squared:  0.6823

mod$coefficients   #gives Bo and B1

mod$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred<-predict(mod)   #store predicted values in the pred
pred

sqrt(mean(mod$residuals^2))         ##RMSE = 2.79165

confint(mod,level=0.95)
predict(mod,interval="predict")
pred1 <- predict(mod,interval="predict")
pred1

library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

###########################################################
###########################################################
# Logrithamic Model

# Model No.- 2 -  x = log(Sorting.Time); y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time

plot(log(X), Y)
cor(log(X), Y)    #correlation value = 0.8339325

mod_log <- lm(Y ~ log(X))   # lm(Y ~ X)

summary(mod_log)     ## R-Squered:- 0.6954

pred<-predict(mod_log)    #store predicted values in the pred
pred

mod_log$residuals     #gives the error value of each record i.e. difference between pred and actual value

sqrt(mean(mod_log$residuals^2))         ##RMSE = 2.733171

confint(mod_log,level=0.95)
predict(mod_log,interval="confidence")
pred1 <- predict(mod_log,interval="confidence")
pred1

library(ggplot2)

ggplot(data = DT, aes(x = log(X), y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=log(X), y=pred))

####################################################################

# Model No.- 3 -  x = Sorting.Time; y = log(Delivery.Time)


X <- Sorting.Time; Y <- Delivery.Time


plot(X, log(Y))
cor(X, log(Y))     #correlation value :- 0.8431773

mod_log1 <- lm(log(Y) ~ X)   # lm(Y ~ X)

summary(mod_log1)    # R-Squered :- 0.7109

tpred<-predict(mod_log1)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log1$residuals     #gives the error value of each record i.e. difference between pred and actual value

# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))    #RMSE value :-  2.94025
RMSE

confint(mod_log1,level=0.95)
predict(mod_log1,interval="confidence")

library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

#########################################################

# Model No.- 4 -  x = log(Sorting.Time); y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(log(X), log(Y))
cor(log(X), log(Y))      #correlation value :- 0.8787271

mod_log2 <- lm(log(Y) ~ log(X))   # lm(Y ~ X)

summary(mod_log2)    # R_Squered :- 0.7722

tpred<-predict(mod_log2)    #store predicted values in the tpred
tpred
pred <- exp(tpred)
pred
mod_log2$residuals     #gives the error value of each record i.e. difference between pred and actual value



# Finding RMSE manually
RMSE <- (sqrt(mean((Y - pred)^2)))   # RMSE value :- 2.745829
RMSE

confint(mod_log2,level=0.95)
predict(mod_log2,interval="confidence")

library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x= X, y= pred))

######################################################################
######################################################################
# Model No.- 5 -  Polynomial model with 2 degree (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 ; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(X , Y)
plot(X*X , Y)

#Lets check the correlation between the given X and Y

cor(X*X , Y)    #correlation value = 0.7939063

#Lets create the model for predicting the Y value

mod_poly2 <- lm(Y ~ X + I(X*X))

summary(mod_poly2)    # R-squared:  0.6934

mod_poly2$coefficients   #gives Bo and B1

mod_poly2$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly2$fitted.values   #store predicted values in the pred
pred

mod_poly2$fitted.values

sqrt(mean(mod_poly2$residuals^2))         ##RMSE = 2.742148   

confint(mod_poly2,level=0.95)
predict(mod_poly2,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

##################################################################

# Model No.- 6 -  Polynomial model with 2 degree with logarithmic Y (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 ; y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(X , log(Y))
plot(X*X , log(Y))

#Lets check the correlation between the given X and Y

cor(X*X , log(Y))    #correlation value = 0.7882452

#Lets create the model for predicting the Y value

mod_poly2L <- lm(log(Y) ~ X + I(X*X))

summary(mod_poly2L)    # R-squared:  0.7649

mod_poly2L$coefficients   #gives Bo and B1

mod_poly2L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly2L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE =  2.799042

confint(mod_poly2L,level=0.95)
predict(mod_poly2L,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

#####################################################################
#####################################################################

# Model No.- 7 -  Polynomial model with 3 degree (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 ; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
#Lets check the correlation between the given X and Y

cor(X^3 , Y)    #correlation value = 0.7540763

#Lets create the model for predicting the Y value

mod_poly3 <- lm(Y ~ X + I(X*X) + I(X^3))

summary(mod_poly3)    # R-squared:  0.7034

mod_poly3$coefficients   #gives Bo and B1

mod_poly3$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly3$fitted.values   #store predicted values in the pred
pred

mod_poly3$fitted.values

sqrt(mean(mod_poly3$residuals^2))         ##RMSE = 2.697085

confint(mod_poly3,level=0.95)
predict(mod_poly3,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

##################################################################

# Model No.- 8 -  Polynomial model with 3 degree with logarithmic Y (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 ; y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^3 , log(Y))    #correlation value = 0.7317283

#Lets create the model for predicting the Y value

mod_poly3L <- lm(log(Y) ~ X + I(X*X) + I(X^3))

summary(mod_poly3L)    # R-squared:  0.7819

mod_poly3L$coefficients   #gives Bo and B1

mod_poly3L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly3L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 2.706757

confint(mod_poly3L,level=0.95)
predict(mod_poly3L,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))
################################################################
################################################################
# Model No.- 9 -  Polynomial model with 4 degree (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4 ; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
#Lets check the correlation between the given X and Y

cor(X^4 , Y)    #correlation value = 0.7161257

#Lets create the model for predicting the Y value

mod_poly4 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4))

summary(mod_poly4)    # R-squared:  0.7034

mod_poly4$coefficients   #gives Bo and B1

mod_poly4$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly4$fitted.values   #store predicted values in the pred
pred

mod_poly4$fitted.values

sqrt(mean(mod_poly4$residuals^2))         ##RMSE = 2.697025

confint(mod_poly4,level=0.95)
predict(mod_poly4,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

################################################################
# Model No.- 10 -  Polynomial model with 4 degree with logarithmic Y (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4; y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^4 , log(Y))    #correlation value = 0.6823162

#Lets create the model for predicting the Y value

mod_poly4L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4))

summary(mod_poly4L)    # R-squared:   0.7825

mod_poly4L$coefficients   #gives Bo and B1

mod_poly4L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly4L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 2.700336

confint(mod_poly4L,level=0.95)
predict(mod_poly4L,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

################################################################
################################################################

# Model No.- 11 -  Polynomial model with 5 degree (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4 + (Sorting.Time)^5 ; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
#Lets check the correlation between the given X and Y

cor(X^5 , Y)    #correlation value = 0.683543

#Lets create the model for predicting the Y value

mod_poly5 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5))

summary(mod_poly5)    # R-squared:  0.7142

mod_poly5$coefficients   #gives Bo and B1

mod_poly5$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly5$fitted.values   #store predicted values in the pred
pred

mod_poly5$fitted.values

sqrt(mean(mod_poly5$residuals^2))         ##RMSE = 2.647535

confint(mod_poly5,level=0.95)
predict(mod_poly5,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

##########################################################################

# Model No.- 12 -  Polynomial model with 5 degree with logarithmic Y (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4 + (Sorting.Time)^5; y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
plot(X^5 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^5 , log(Y))    #correlation value = 0.6420312

#Lets create the model for predicting the Y value

mod_poly5L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5))

summary(mod_poly5L)    # R-squared:   0.7884

mod_poly5L$coefficients   #gives Bo and B1

mod_poly5L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly5L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 2.656727

confint(mod_poly5L,level=0.95)
predict(mod_poly5L,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

####################################################################
####################################################################

# Model No.- 13 - Polynomial model with 6 degree (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4 + (Sorting.Time)^5 + (Sorting.Time)^6 ; y = Delivery.Time

X <- Sorting.Time; Y <- Delivery.Time


plot(X , Y)
plot(X*X , Y)
plot(X^3 , Y)
plot(X^4 , Y)
plot(X^5 , Y)
plot(X^6 , Y)
#Lets check the correlation between the given X and Y

cor(X^6 , Y)    #correlation value = 0.6566074

#Lets create the model for predicting the Y value

mod_poly6 <- lm(Y ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5) + I(X^6))

summary(mod_poly6)    # R-squared:   0.7143

mod_poly6$coefficients   #gives Bo and B1

mod_poly6$residuals   #gives the error value of each record i.e. difference between pred and actual value

pred <- mod_poly6$fitted.values   #store predicted values in the pred
pred

mod_poly6$fitted.values

sqrt(mean(mod_poly6$residuals^2))         ##RMSE = 2.647117

confint(mod_poly6,level=0.95)
predict(mod_poly6,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))


######################################################################

# Model No.- 14 -  Polynomial model with 6 degree with logarithmic Y (quadratic model)

# x = Sorting.Time + (Sorting.Time)^2 + (Sorting.Time)^3 + (Sorting.Time)^4 + (Sorting.Time)^5 + (Sorting.Time)^6; y = log(Delivery.Time)

X <- Sorting.Time; Y <- Delivery.Time


plot(X , log(Y))
plot(X*X , log(Y))
plot(X^3 , log(Y))
plot(X^4 , log(Y))
plot(X^5 , log(Y))
plot(X^6 , log(Y))
#Lets check the correlation between the given X and Y

cor(X^6 , log(Y))    #correlation value = 0.6099308

#Lets create the model for predicting the Y value

mod_poly6L <- lm(log(Y) ~ X + I(X*X) + I(X^3) + I(X^4) + I(X^5) + I(X^6))

summary(mod_poly6L)    # R-squared:   0.7885

mod_poly6L$coefficients   #gives Bo and B1

mod_poly6L$residuals   #gives the error value of each record i.e. difference between pred and actual value

tpred <- mod_poly6L$fitted.values   #store predicted values in the pred
tpred

pred <- exp(tpred)
pred


sqrt(mean((pred - Y)^2))         ##RMSE = 2.658473

confint(mod_poly6L,level=0.95)
predict(mod_poly6L,interval="predict")


library(ggplot2)

ggplot(data = DT, aes(x = X, y = Y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=X, y=pred))

######################################################################
####################################################################

# Answer:- Above are the models which we have created for predicting the delivery time.
# From the data shown in the Q2 Excel datasheet we can conclude that the model no. 12 is the best model with which we have got the best results so far
# This result is based on the values of Correlation, R-Squared value and RMSE of the given data to get the most optimum result.
# Hence we are going to use the model no. 12 for the required predictions.