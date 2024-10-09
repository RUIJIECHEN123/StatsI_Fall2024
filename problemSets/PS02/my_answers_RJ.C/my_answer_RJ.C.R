pchisq(4.3193,df=2,lower.tail=FALSE) #answer:0.1153655
RJC <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
model <- lm(water ~ reserved, data = RJC)
summary(model)
#Call:
#  lm(formula = water ~ female, data = reserved)

#Residuals:
# Min     1Q Median     3Q    Max 
#-22.68 -14.78  -7.81   2.29 317.32 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   14.813      2.382   6.220 1.56e-09 ***
#  female         7.864      3.838   2.049   0.0413 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 33.51 on 320 degrees of freedom
#Multiple R-squared:  0.01295,	Adjusted R-squared:  0.009867 
#F-statistic: 4.199 on 1 and 320 DF,  p-value: 0.04126