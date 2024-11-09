#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

library(ggplot2)
df <- read.csv("C:/Users/80714/Desktop/StatsI_Fall2024/problemSets/PS03/my_answers/incumbents_subset.csv", header = TRUE)
model <- lm(voteshare ~ difflog, data = df)
summary(model)#Output regression analysis results
ggplot(df, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression Analysis of Vote Share on Difflog",
       x = "Difference in Log of Spending (difflog)", 
       y = "Vote Share (voteshare)")#Draw regression lines and scatter plots
residuals <- residuals(model)
head(residuals)
plot(residuals)
intercept <- coef(model)[1]#Extracted is intercept
slope <- coef(model)[2]#Extracted is slope
prediction_equation <- paste("voteshare =", intercept, " + ", slope, "* difflog")
print(prediction_equation)

#question2
model_presvote <- lm(presvote ~ difflog, data = df)
summary(model_presvote)#Output regression analysis results
ggplot(df, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression Analysis of Presvote on Difflog",
       x = "Difference in Log of Spending (difflog)", 
       y = "Presidential Vote Share (presvote)")#Draw regression lines and scatter plots
residuals_presvote <- residuals(model_presvote)
head(residuals_presvote)
plot(residuals_presvote)
intercept_presvote <- coef(model_presvote)[1]#Extracted is intercept
slope_presvote <- coef(model_presvote)[2]#Extracted is slope
prediction_equation_presvote <- paste("presvote =", intercept_presvote, " + ", slope_presvote, "* difflog")
print(prediction_equation_presvote)

#question3
model_voteshare <- lm(voteshare ~ presvote, data = df)
summary(model_voteshare)#Output regression analysis results
ggplot(df, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression Analysis of Vote Share on Presidential Vote",
       x = "Presidential Vote Share (presvote)", 
       y = "Vote Share (voteshare)")#Draw regression lines and scatter plots
intercept_voteshare <- coef(model_voteshare)[1]#Extracted is intercept
slope_voteshare <- coef(model_voteshare)[2]#Extracted is slope
prediction_equation_voteshare <- paste("voteshare =", intercept_voteshare, " + ", slope_voteshare, "* presvote")
print(prediction_equation_voteshare)

#question4
df$residuals <- residuals
df$residuals_presvote <- residuals_presvote
model_residuals <- lm(residuals ~ residuals_presvote, data = df)
summary(model_residuals)#Output regression analysis results
ggplot(df, aes(x = residuals_presvote, y = residuals)) +
  geom_point() +  
  geom_smooth(method = "lm", color = "blue") +  
  labs(title = "Scatterplot of Residuals with Regression Line",
       x = "Residuals from Presidential Vote Regression (residuals_presvote)", 
       y = "Residuals from Vote Share Regression (residuals)")#Draw regression lines and scatter plots
intercept_residuals <- coef(model_residuals)[1]#Extracted is intercept
slope_residuals <- coef(model_residuals)[2]#Extracted is slope
prediction_equation <- paste("residuals =", intercept_residuals, " + ", slope_residuals, " * residuals_presvote")
print(prediction_equation)

#question5
model_voteshare <- lm(voteshare ~ difflog + presvote, data = df)
summary(model_voteshare)#Output regression analysis results
intercept <- coef(model_voteshare)[1]
coef_difflog <- coef(model_voteshare)[2]
coef_presvote <- coef(model_voteshare)[3]
prediction_equation <- paste("voteshare =", intercept, 
                             " +", coef_difflog, "* difflog",
                             " +", coef_presvote, "* presvote")
print(prediction_equation)