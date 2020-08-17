#Linear regression
data(airquality)
View(airquality)
names(airquality)

#[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"  

plot(Ozone~Solar.R, data=airquality)

#null hypothesis, there is no relation between Ozone and Solar radiation
#need to calculate mean
mean(airquality$Ozone) #NA because of nas in the dataset
airquality$Ozone

ozone_mean= mean(airquality$Ozone, na.rm=T) 

#horizontal line at the mean
abline(h=ozone_mean)

#use lm to fit a regression line
model_lm= lm(Ozone~Solar.R, data=airquality)
model_lm

abline(model_lm, col="red")

#observation, with higher values of radiation, the spread of ozone values increases

#plot the model
plot(model_lm)
#https://onlinecourses.science.psu.edu/stat501/node/250

summary(model_lm)
#The p-value for each term tests the null hypothesis that the coefficient is equal to zero (no effect). 
# A low p-value (< 0.05) indicates that you can reject the null hypothesis. 
# In other words, a predictor that has a low p-value is likely to be a meaningful addition to your 
#model because changes in the predictor's value are related to changes in the response variable.

#median should be zero
#t values = estimate std./error
#Probablity of  value > .... given t distibution.....quite sure that intercept is not zero
#p-value 

#The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data 
#plausibly came from some theoretical distribution such as a Normal or exponential.

##########################################################################

#plot of explanotary variable(s)
plot(Ozone~Wind,airquality)

model_lm2=lm(Ozone~Solar.R*Wind, airquality)

plot(model_lm2)

#graphically see the results
termplot(model_lm2)
summary(model_lm2)


#Warning in termplot(model_lm2) :
#'model' appears to involve interactions: see the help page


##########################################################################
summary(airquality$Solar.R)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 #   7.0   115.8   205.0   185.9   258.8   334.0       7 


summary(airquality$Wind)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.700   7.400   9.700   9.958  11.500  20.700 

Solar1= mean(airquality$Solar.R, na.rm=TRUE)
Solar1
Solar2=100
Solar3=300

#single value
predict(model_lm2, data.frame(Solar.R=100, Wind=10))

#set of values
p1=predict(model_lm2, data.frame(Solar.R=Solar1, Wind=1:20))
p2=predict(model_lm2, data.frame(Solar.R=Solar2, Wind=1:20))
p3=predict(model_lm2, data.frame(Solar.R=Solar3, Wind=1:20))


######################
plot(Ozone~Wind, airquality)
lines(1:20, p1, col="red")
lines(1:20, p2, col="blue")
lines(1:20, p3, col="yellow")



######################
plot(Ozone~Wind, data=airquality)
model_OW=lm(Ozone~Wind, data=airquality)
plot(model_OW)

coef(model_OW)
#(Intercept)        Wind 
#96.872895   -5.550923 

#predictions for Wind sppeds of 19 and 20
Ozone1<-coef(model_OW)[1]+coef(model_OW)[2]*19
Ozone2<-coef(model_OW)[1]+coef(model_OW)[2]*20

Ozone1
Ozone2