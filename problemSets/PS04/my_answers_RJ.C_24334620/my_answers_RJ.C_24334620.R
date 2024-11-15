install.packages("car")
library(car)
data(Prestige)
help(Prestige)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)

prestige_model <- lm(prestige ~ income * professional, data = Prestige)
summary(prestige_model)
