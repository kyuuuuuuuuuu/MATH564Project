# Math 564 project
# read data
setwd("C:/Users/Jin/Desktop/MATH564Project")
myD<-read.csv("kc_house_data.csv",stringsAsFactors = FALSE)
summary(myD)
str(myD)
myD$date <- as.Date(substring(myD$date, 0, 8), "%Y%m%d")
myD <- myD[, -1]
write.csv(myD, file = "kc_house_data_cleaned.csv")

# pair matrix without dates
# library(lattice)
# splom(~myD[,-1])



# approach the data with a easy linear model
basic_fit <- lm(price~., data = myD)
summary(basic_fit)
