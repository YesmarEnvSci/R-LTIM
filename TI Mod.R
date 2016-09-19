## TI Model

op <-par(mfrow=c(1,1)) #default plot window
lidarData <- read.csv("BA_lidarData.csv")
lidarData = lidarData[,-1]
library(leaps)
BAregsubsets = regsubsets(BAtotal ~., data = lidarData, nbest=4, nvmax=3, method = 'seqrep')
summary(BAregsubsets)
## Use the regsubsets plot to identify best least subset of predictor variables for model fit
plot(BAregsubsets, scale="r2")

## Look for interactin terms
# what predictors are most important affecting BAtotal?  Interaction
library(tree)
modTree <- tree(BAtotal ~., data = lidarData)
par(mfrow=c(1,1))
plot(modTree)
text(modTree) # Compare variables with regsubsets


## Fit a linear model with the predictors
Lm0 = lm(BAtotal ~ P70 + all_1st_cover_mean , data = lidarData)
summary(Lm0)# Both coefficients are signif influencers of BAtotal
par(mfrow = c(2,2))
plot(Lm0) 
par(op)

## Fit another model with interaction term
Lm1 = lm(BAtotal ~ P70*all_1st_cover_mean, data = lidarData)
summary(Lm1)
par(mfrow = c(2,2))
plot(Lm1)  
par(op)
coef(Lm1) #show the model coefficients, copy and paste to text editor or output in Rmarkdown for raster calculator format
range(lidarData$P70)
range(lidarData$all_1st_cover_mean)



library(MASS)
## BoxCox transformation recomendation: if lambda is zero, Log. If 0.5, SqrRt. If 1, linear (no transformation). If -1, Reciprocal
bcLm1 = boxcox(Lm1)  
lambda <- bcLm1$x[which.max(bcLm1$y)]  ## The optimum value and CI are close to 1, no transformatin required

## Is improved model fit woth the added complexity of additional variable?
anova(Lm0, Lm1)  # with a p-value < .05, there is a sig dif, thus worth the added complexity (Lm1 is better than Lm0)

## Compare observed with predicted values
Lm1_BAtotal_pred = Lm1$fitted
plot(Lm1_BAtotal_pred ,lidarData$BAtotal,
     main="Observed vs Predicted",
     ylab="BAtotal observed", xlab="BAtotal predicted",
     ylim=c(0,max(lidarData$BAtotal)), xlim=c(0,max(lidarData$BAtotal)))
abline(0,1,lwd=3, col="darkgreen")

AIC(Lm0,Lm1,k=2) # Looking for the model with the lowest AIC.

## Variance Invlation Factor (VIF) - Test for multicolinearity: > 4-5 sugest a problem, > 10 highly likely
library(car)
vif(Lm0) 
vif(Lm1)


## Model with 3 variables
Lm2 = lm(BAtotal ~ P40*X1st_cover_mean*all_1st_cover_mean, data = lidarData)
summary(Lm2)
par(mfrow=c(2,2))
plot(Lm2) # This model does not have coefficients that are significant predictors
par(op)
