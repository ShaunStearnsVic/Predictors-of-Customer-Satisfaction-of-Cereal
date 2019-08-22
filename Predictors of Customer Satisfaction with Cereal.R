library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(adabag)
library(jtools)
library(sandwich)

setwd("~/Desktop/CSU Global Data Analytics/MIS510/Module 2/")

#Import, view first 9 rows, and see list of variables
Cereals.df <- read.csv("Cereals.csv", header = TRUE)
head(Cereals.df, 9)
t(t(names(Cereals.df))) 

#Limit dataset to quantitative variables and view first 9 rows
Cereals2.df <- Cereals.df[c(4:16)]
head(Cereals2.df, 9)
data.frame(mean=sapply(Cereals2.df, mean, na.rm=TRUE), 
           sd=sapply(Cereals2.df, sd, na.rm=TRUE), 
           min=sapply(Cereals2.df, min, na.rm=TRUE), 
           max=sapply(Cereals2.df, max, na.rm=TRUE), 
           median=sapply(Cereals2.df, median, na.rm=TRUE), 
           length=sapply(Cereals2.df, length),
           miss.val=sapply(Cereals2.df, function(x) 
             sum(length(which(is.na(x))))))

View(Cereals2.df)


#Regression Tree Predicting Customer Satisfaction
Cereals.ct <- rpart(rating ~ ., data = Cereals2.df, method = "anova",
                cp = 0.00001, minsplit = 5, xval = 5)
length(Cereals.ct$frame$var[Cereals.ct$frame$var == "<leaf>"])
printcp(Cereals.ct)

pruned.ct <- prune(Cereals.ct,
                   cp = Cereals.ct$cptable[which.min(Cereals.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, digits=-6)
printcp(pruned.ct)

#Histogram for all quantitative variables
hist(Cereals2.df$calories, xlab = "calories", main = "Quantitative Variables")
hist(Cereals2.df$protein, xlab = "protein", main = "Quantitative Variables")
hist(Cereals2.df$fat, xlab = "fat", main = "Quantitative Variables")
hist(Cereals2.df$sodium, xlab = "sodium", main = "Quantitative Variables")
hist(Cereals2.df$fiber, xlab = "fiber", main = "Quantitative Variables")
hist(Cereals2.df$carbo, xlab = "carbohydrates", main = "Quantitative Variables")
hist(Cereals2.df$sugars, xlab = "sugars", main = "Quantitative Variables")
hist(Cereals2.df$potass, xlab = "potass", main = "Quantitative Variables")
hist(Cereals2.df$vitamins, xlab = "vitamins", main = "Quantitative Variables")
hist(Cereals2.df$shelf, xlab = "shelf", main = "Quantitative Variables")
hist(Cereals2.df$weight, xlab = "weight", main = "Quantitative Variables")
hist(Cereals2.df$cups, xlab = "cups", main = "Quantitative Variables")
hist(Cereals2.df$rating, xlab = "rating", main = "Quantitative Variables")

#Boxplot for Customer satisfaction per shelf height
boxplot(Cereals2.df$rating ~ Cereals2.df$shelf, xlab = "Shelf Height", ylab = "Cust Rating", 
        main = "Customer Satisfaction \n and Shelf Height")
