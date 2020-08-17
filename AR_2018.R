#Association rule mining
#https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf

install.packages("arules")
library(arules)

#AdultUCI data originates from the U.S. census bureau database and contains 48842 instances
#with 14 attributes like age, work class, education, etc.
data("AdultUCI")

dim(AdultUCI)

AdultUCI[1:2,]


#First, we remove the two attributes fnlwgt and education-num. The first attribute is a 
#weight calculated by the creators of the data set from control data provided by the 
#Population Division of the U.S. census bureau. The second removed attribute is just a 
#numeric representation of the attribute education which is also part of the data set.

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

#Next, we need to map the four remaining metric attributes (age, hours-per-week, 
#capital-gain and capital-loss) to ordinal attributes by building suitable categories.

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), 
                              labels = c("Young", "Middle-aged", "Senior", "Old"))


AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],
                        c(0,25,40,60,168)),
                        labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]],
          c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
            labels = c("None", "Low", "High"))


AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
      c(-Inf,0,median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
      labels = c("none", "low", "high"))

#Now, the data can be automatically recoded as a binary incidence matrix by coercing the
#data set to transactions.

Adult <- as(AdultUCI, "transactions")
Adult

summary(Adult)


#To reduce the number of items, we only plot the item frequency for items with a 
#support greater than 10% (using the parameter support). For better readability of 
#the labels, we reduce the label size with the parameter cex.names.
itemFrequencyPlot(Adult, support = 0.2, cex.names=0.8)


#Call the function apriori() to find all rules (the default association type for
#apriori()) with a minimum support of 1% and a confidence of 0.6.
rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))

rules
summary(rules)

#As typical for association rule mining, the number of rules found is huge. 
#To analyze these rules, for example, subset() can be used to produce separate subsets 
#of rules for each item which resulted from the variable income in the right-hand-side 
#of the rule. At the same time we require that the lift measure exceeds 1.2.

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)

#We now have a set with rules for persons with a small income and a set for persons with
#a large income. For comparison, we inspect for both sets the three rules with the highest
#confidence (using head()).

inspect(head(rulesIncomeSmall, n = 3, by = "confidence"))
inspect(head(rulesIncomeLarge, n = 3, by = "confidence"))


write(rulesIncomeSmall, file = "data.csv", sep = ",", col.names = NA)






