# Math 564 project
library(ggplot2)
library(reshape2)
library(MASS)
library(leaps)


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

# select best model by adj R^2
best <- function(model, ...) 
{
    subsets <- regsubsets(formula(model), model.frame(model), ...)
    subsets <- with(summary(subsets),
                    cbind(p = as.numeric(rownames(which)), which, adjr2))
    
    return(subsets)
}  
##############################################


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
# price has high correlation with sqft_living, grade, sqft_above, sqft_living15, and bathrooms.
melted_cormat_byValue <- melted_cormat[order(abs(melted_cormat$value), decreasing = T),]
melted_cormat_byValue_price <- melted_cormat_byValue[melted_cormat_byValue$Var1=='price' |
                                                         melted_cormat_byValue$Var2=='price',]


## boxplots for some high-correlated variables and price
boxplot_p_sl <- boxplot(price~sqft_living, data=myD, 
                 main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")
boxplot_p_b <- boxplot(price~bathrooms, data=myD, 
                    col=(c("gold","darkgreen")),
                    main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")
boxplot_p_g <- boxplot(price~grade, data=myD, 
                       col=(c("gold","darkgreen")),
                       main="Price vs. Grade", xlab="Grade", ylab="Price")

## full linear model
full.model <- lm(price~., data = myD)
summary(full.model)
# coef of sqft_basement is NA
# R^2 = 0.6997, adR^2 = 0.6995
fit_drop_basement <- lm(price~.-sqft_basement, data = myD)
summary(fit_drop_basement)
# R^2 = 0.6997, adR^2 = 0.6995

## simplified model
sim_fit <- lm(price~sqft_living+grade+sqft_above+sqft_living15+bathrooms, data = myD)
summary(sim_fit)
# R^2 = 0.5442, adR^2 = 0.5441
# select model by adjusted R^2
# fit2 <- lm(Y~X1+X2+X3+I(X1^2)+I(X2^2)+I(X3^2)+I(X1*X2)+I(X1*X3)+I(X2*X3), data = myData1)
result <- as.data.frame(round(best(fit_drop_basement, nbest = 6), 4))
result <- result[order(-abs(result$adjr2)),] 
head(result)
# So best model by adjusted R^2 is 
# price ~ bedrooms + bathrooms + sqft_living + waterfront + view + grade + yr_built + lat
# top models by adjusted R^2 should contain 
# bedrooms, bathrooms, sqft_living, waterfront, view, grade, yr_built and lat.

## model with second-order terms
second_order_fit <- NULL

## backward variable selection
model.null <- lm(price ~ 1, data = myD)
model.backward <- step(fit_drop_basement, scope = list(lower = model.null), 
                       direction = "backward", test = "Chisq", data = myD, trace = F)
model.backward$formula



