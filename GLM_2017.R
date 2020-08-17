#Generealized Linear Regression
data("airquality")

plot(Ozone~Wind, airquality)

model1=lm(Ozone~Wind, data=airquality)
coef(model1)

#prediction of Ozone for Wind speed of 19 and 20
Ozone1= coef(model1)[1]+coef(model1)[2]*19
Ozone2= coef(model1)[1]+coef(model1)[2]*20

model2=glm(Ozone~Wind, data=airquality, family=poisson)

coef(model2)

Ozone1.glm= exp(coef(model2)[1]+coef(model2)[2]*19)
Ozone2.glm= exp(coef(model2)[1]+coef(model2)[2]*20)

Ozone3.glm= exp(coef(model2)[1]+coef(model2)[2]*5)
Ozone4.glm= exp(coef(model2)[1]+coef(model2)[2]*6)


Ozone1.glm
Ozone2.glm

Ozone2.glm/Ozone1.glm #exp(-0.1488753 )

#In a GLM, an individual slope gives an estimate of the multiplicative change 
#in the response variable for a one unit change in the corresponding explanatory variable


#############################################################################
#Another example: A researcher is interested in how variables, such as GRE 
#(Graduate Record Exam scores), GPA (grade point average) and prestige of 
#the undergraduate institution, effect admission into graduate school. 
#The response variable, admit/don't admit, is a binary variable.


library(aod)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

#admit: binary, categorical
#gre:continuous
#gpa:continuous
#rank: 4 different levels (1) being the top

summary(mydata)

#To get the standard deviations, we use sapply
sapply(mydata,sd)

## two-way contingency table of categorical outcome and predictors. We want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

#convert rank to a factor to indicate that rank should be treated as a 
#categorical variable.
mydata$rank <- factor(mydata$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)
plot(mylogit)

#The logistic regression coefficients give the change in the log odds of 
#the outcome for a one unit increase in the predictor variable.

#For every one unit change in gre, the log odds of admission (versus non-admission) 
#increases by 0.002.

#For a one unit increase in gpa, the log odds of being admitted to graduate school 
#increases by 0.804.

#The indicator variables for rank have a slightly different interpretation. 
#For example, having attended an undergraduate institution with rank of 2, 
#versus an institution with a rank of 1, changes the log odds of admission by -0.675.


#########################################################################################


newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

#More information: https://stats.idre.ucla.edu/r/dae/logit-regression/

#Logistic regression function
#z=seq(-8,8, by=.1)
#f=1/(1+exp(-z))
#plot(z,f)