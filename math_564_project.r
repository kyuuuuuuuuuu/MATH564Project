# Math 564 project
# read data
library(ggplot2)
library(reshape2)


##set up
# setwd("C:/Users/Jin/Desktop/MATH564Project")
myD<-read.csv("./kc_house_data.csv",stringsAsFactors = FALSE)
summary(myD)
str(myD)
#the data is pretty clean. No NAs and most of them are int/num.
#we can just drop the first two columns, id and dates.
myD <- myD[, -c(1,2)]

##correlations
# pair matrix without dates
cormat <- cor(myD)
round(cormat, 2)
melted_cormat <- melt(cormat)
cormat <- reorder_cormat(cormat)
upper_tri <- get_lower_tri(cormat)
# upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# create correlation heatmap
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()


# sqft_living, sqft_above and grades are highly related.
# price has high correlation with sqft_living, grade, sqrt_above, and bathrooms.
melted_cormat_byValue <- melted_cormat[order(abs(melted_cormat$value), decreasing = T),]
melted_cormat_byValue_price1 <- melted_cormat_byValue[melted_cormat_byValue$Var1=='price',]
melted_cormat_byValue_price2 <- melted_cormat_byValue[melted_cormat_byValue$Var2=='price',]

## boxplots for some high-correlated variables and price
boxplot_p_sl <- boxplot(price~sqft_living, data=myD, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")
boxplot_p_b <- boxplot(price~bathrooms, data=myD, 
                    col=(c("gold","darkgreen")),
                    main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")
boxplot_p_g <- boxplot(price~grade, data=myD, 
                       col=(c("gold","darkgreen")),
                       main="Price vs. Grade", xlab="Grade", ylab="Price")



## full linear model
basic_fit <- lm(price~., data = myD)
summary(basic_fit)
# coef of sqft_basement is NA
fit_drop_basement <- lm(price~.-sqft_basement, data = myD)
summary(fit_drop_basement)
# R^2 = 0.6997, adR^2 = 0.6995

## simplified model
sim_fit <- lm(price~sqft_living+bathrooms+grade, data = myD)
summary(sim_fit)
# R^2 = 0.5371, adR^2 = 0.5371

## model with second-order terms
second_order_fit <- 

## backward variable selection
#backward
model.null <- lm(price~., data = myD)
fit2.BIC <- step(fit_drop_basement, direction = "backward", k=log(nrow(myD)))
fit2.BIC$formula



##############################################
# helper functions
# get lower triangle of the matrix
get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
}

# reorder the correlation values
reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
}