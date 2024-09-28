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
lapply(c(),  pkgTest)
# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")
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
lapply(c("ggplot2"),  pkgTest)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# load data as vector
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
# capture the number of observations
n <- length(y)
# Calculate the 90% confidence interval for the student IQ
# Step 1: get t-score
t <- qt(0.05, n-1, lower.tail = F)
# Step 2: Calculate lower and upper parts for the 90%
lower_CI <- mean(y)-(t*(sd(y)/sqrt(n)))
upper_CI <- mean(y)+(t*(sd(y)/sqrt(n)))
# print CIs with mean
c(lower_CI, mean(y), upper_CI) #Confidence interval (93.95993 102.92007) mean value(98.44000)
# double check our answer  
t.test(y, conf.level = 0.9)$"conf.int" #Use the t.test() function to directly calculate the 90% confidence interval and extract the confidence interval
# Calculate the standard error
SE <- sd(y)/sqrt(n)
# Calculate the test statistic for this hypothesis testing of mean
t <- (mean(y) - 100)/SE
# Get the p-value from t-distribution
pvalue <- pt(t, n-1, lower.tail = F)
# Or another way to do this hypothesis testing is to use the function t.test directly
t.test(y, mu = 100, conf.level = 0.95, alternative = "greater")
#               One Sample t-test
#data:  y
#t = -0.59574, df = 24, p-value = 0.7215
#(The t-value is close to 0, indicating that there is not much difference between the sample mean and the assumed mean (100))
#(The p-value is much greater than 0.05, which means there is not enough evidence to reject the null hypothesis, i.e. there is no evidence to suggest that the sample mean is significantly greater than 100)
#alternative hypothesis: true mean is greater than 100
#(Indicating the hypothesis that the sample mean is greater than 100)
#95 percent confidence interval:
#  93.95993      Inf
#(The lower limit of the confidence interval is 93.95993.The upper limit of the confidence interval is infinite)
#sample estimates:
#mean of x 
#   98.44
#(The sample mean is 98.44)



# read in expenditure data
expenditure <- read.table("expenditure.txt", header=T)
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
lapply(c("ggplot2", "stargazer"),  pkgTest)
# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# read in expenditure data
# *Need to install and load the HTTR package first
# if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
# library(httr)
# url <- "https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt"
# response <- GET(url)
# if (response$status_code == 200) {
#             content <- content(response, "text")
#             lines <- unlist(strsplit(content, "\n"))
#             expenditure <- read.table(text = paste0(lines, collapse = "\n"), header = TRUE)
# } else {
#         stop("Failed to download the file: HTTP status ", response$status_code)
# }
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T) 
pairs(expenditure[, c("Y", "X1", "X2", "X3")]) #Draw a scatter plot of Y with X1, X2, X3
RJ.C <- cor(expenditure[, c("Y", "X1", "X2", "X3")]) #Calculate correlation
print(RJ.C) 
summary(expenditure) #Output the statistical results as a text file
sink("summary.txt")
print(summary(expenditure))
sink() #complete the first question 
install.packages("ggplot2") #Install ggplot2 package to draw charts
library(ggplot2) #Load ggplot2 package
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
head(expenditure) #Check the first six items of the read webpage text
ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Relationship between Y and Region",
       x = "Region",
       y = "Y (Per Capita Expenditure on Housing Assistance)")  #Draw a point of Y and Region
pdf("plot.Y.Region_RJ.C.pdf")
plot(expenditure$Region,expenditure$Y)
dev.off() #Complete the first question of the second question
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
average_expenditure <- aggregate(Y ~ Region, data=expenditure, FUN=mean)
highest_region <- average_expenditure[which.max(average_expenditure$Y),]
print(highest_region)#Complete the second question of the second question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X1", x = "X1", y = "Y")  #Draw a point of Y and X1
pdf("plot.Y.X1_RJ.C.pdf")
plot(expenditure$X1,expenditure$Y)
dev.off()  #Complete the first question(Y/X1) of the third question    
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(Y ~ X1, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
output_stargazer("regression_output_RJ.C.tex", regression_model) #This will write the output of stargazer to the 'regression_output_RJ.C.tex' file #Complete the second question(Y/X1) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X1 by Region", x = "X1", y = "Y") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors_RJ.C.pdf")
plot(expenditure$X1,expenditure$Y)
dev.off()   #Complete the third question(Y/X1) of the third question  

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X2", x = "X2", y = "Y")  #Draw a point of Y and X2
pdf("plot.Y.X2_RJ.C.pdf")
plot(expenditure$X2,expenditure$Y)
dev.off()  #Complete the first question(Y/X2) of the third question   
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(Y ~ X2, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
library(stargazer)
output_stargazer("regression_output2_RJ.C.tex", regression_model) #Complete the second question(Y/X2) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X2, y = Y, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X2 by Region", x = "X2", y = "Y") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors2_RJ.C.pdf")
plot(expenditure$X2,expenditure$Y)
dev.off()   #Complete the third question(Y/X2) of the third question 

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X3", x = "X3", y = "Y")  #Draw a point of Y and X3
pdf("plot.Y.X3_RJ.C.pdf")
plot(expenditure$X3,expenditure$Y)
dev.off()  #Complete the first question(Y/X3) of the third question
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(Y ~ X3, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
library(stargazer)
output_stargazer("regression_output3_RJ.C.tex", regression_model) #Complete the second question(Y/X3) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X3, y = Y, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of Y vs. X3 by Region", x = "X3", y = "Y") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors3_RJ.C.pdf")
plot(expenditure$X3,expenditure$Y)
dev.off()   #Complete the third question(Y/X3) of the third question

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X2, y = X1)) +
  geom_point() +
  labs(title = "Scatterplot of X1 vs. X2", x = "X2", y = "X1")  #Draw a point of X1 and X2
pdf("plot.X1.X2_RJ.C.pdf")
plot(expenditure$X2,expenditure$X1)
dev.off()#Complete the first question(X1/X2) of the third question
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(X1 ~ X2, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
library(stargazer)
output_stargazer("regression_output4_RJ.C.tex", regression_model) #Complete the second question(X1/X2) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X2, y = X1, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of X1 vs. X2 by Region", x = "X2", y = "X1") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors4_RJ.C.pdf")
plot(expenditure$X2,expenditure$X1)
dev.off()   #Complete the third question(X1/X2) of the third question

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X3, y = X1)) +
  geom_point() +
  labs(title = "Scatterplot of X1 vs. X3", x = "X3", y = "X1")  #Draw a point of X1 and X3
pdf("plot.X1.X3_RJ.C.pdf")
plot(expenditure$X3,expenditure$X1)
dev.off()#Complete the first question(X1/X3) of the third question
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(X1 ~ X3, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
library(stargazer)
output_stargazer("regression_output5_RJ.C.tex", regression_model) #Complete the second question(X1/X3) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X3, y = X1, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of X1 vs. X3 by Region", x = "X3", y = "X1") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors5_RJ.C.pdf")
plot(expenditure$X3,expenditure$X1)
dev.off()   #Complete the third question(X1/X3) of the third question

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
ggplot(expenditure, aes(x = X3, y = X2)) +
  geom_point() +
  labs(title = "Scatterplot of X2 vs. X3", x = "X3", y = "X2")  #Draw a point of X2 and X3
pdf("plot.X2.X3_RJ.C.pdf")
plot(expenditure$X3,expenditure$X2)
dev.off()#Complete the first question(X2/X3) of the third question
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
regression_model <- lm(X2 ~ X3, data=expenditure)
summary(regression_model)
output_stargazer <- function(outputFile, model) {
  output <- capture.output(stargazer(model, type = "text"))
  writeLines(output, con = outputFile)
}
library(stargazer)
output_stargazer("regression_output6_RJ.C.tex", regression_model) #Complete the second question(X2/X3) of the third question
library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
ggplot(expenditure, aes(x = X3, y = X2, color = Region, shape = Region)) +
  geom_point() +
  labs(title = "Scatterplot of X2 vs. X3 by Region", x = "X3", y = "X2") +
  theme_minimal() +
  scale_color_manual(values = c("dimgray", "gold", "red", "blue", "coral")) +
  scale_shape_manual(values = c(18, 23, 10, 17, 2))
pdf("plot.symbols.colors6_RJ.C.pdf")
plot(expenditure$X3,expenditure$X1)
dev.off()   #Complete the third question(X2/X3) of the third question
#my_answers_RUIJIE CHEN_24334620