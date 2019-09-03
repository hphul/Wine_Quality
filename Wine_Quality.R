###Project Practice

###DATA Import
WhiteWine<-read.table("D:/Pablo/Documents/STATS512Project/winequality-white.csv", header =TRUE, sep=",")

###Full Model
library(alr4)
White.mod <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + 
                  total.sulfur.dioxide + density + pH + sulphates + alcohol, data = WhiteWine)
summary(White.mod)
qqplot(White.mod)

###Determining Best Predictors
library(leaps)
best_subsets <-regsubsets(quality ~., data = WhiteWine)
which.max(summary(best_subsets)$adjr2)
plot(best_subsets, scale = "r2")
library(MASS)
stepAIC(White.mod,direction = "both")$anova

### Best Predictors: fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + sulphates + alcohol
### Remove Predictors: citric.acid + chlorides + total.sulfur.dioxide

###Best Model
White.mod1 <- lm(quality ~ fixed.acidity + volatile.acidity  + residual.sugar + free.sulfur.dioxide
                 + density + pH + sulphates + alcohol, data = WhiteWine)

summary(White.mod1)
anova(White.mod1)
### plotting Mod.1
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(White.mod1)

###Checking for Factors
length(unique(WhiteWine$fixed.acidity))
length(unique(WhiteWine$volatile.acidity))
length(unique(WhiteWine$residual.sugar))
length(unique(WhiteWine$free.sulfur.dioxide))
length(unique(WhiteWine$density))
length(unique(WhiteWine$pH))
length(unique(WhiteWine$sulphates))
length(unique(WhiteWine$alcohol))
length(WhiteWine$quality)
hist(WhiteWine$quality)
###confidence intervals
confint(White.mod1)


###Checking For Assumptions
library(car)
Resid.1 <- resid(White.mod1);
fitted.1 <- predict(White.mod1);fitted.1



### Normality of Residuals
qqPlot(Resid.1, main = "QQ Plot of Residuals")
shapiro.test(Resid.1)
hist(Resid.1)
plot(Resid.1)

### Homoscedasticity (constant variance)
plot(x = fitted.1, y = Resid.1, main = "Scatter plot of Residual vs Fitted Va
     lue", xlab = "Fitted Value", ylab = "Residual")
abline(0,0)

### Independence of residuals
plot(x = c(1:length(Resid.1)), y = Resid.1, main = "Scatter plot of Residual
     vs Observation Order", xlab = "Observation Order", ylab = "Residual", type =
       'b')
abline(0,0)

### Cooks Distance
cookdist <- cooks.distance(White.mod1); cookdist
outlierTest(White.mod1)

##### CHECK for Multicollinearity
library(MASS)
vif(White.mod1) ###VIF 0f 26.12 for Density so remove

#### Creating new Model without Density
White.mod2<- lm(quality ~ fixed.acidity + volatile.acidity  + residual.sugar + free.sulfur.dioxide
                 + pH + sulphates + alcohol, data = WhiteWine)
summary(White.mod2)

### plotting Mod.2
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(White.mod2)

### Check for Multicollinearity for Mod.2
vif(White.mod2)

### confidence intervals
confint(White.mod2)
predict(White.mod2, newdata = WhiteWine, interval = "predict")

### Checking Assumptions
library(gvlma)
gvlma(White.mod2)
### Checking Outliers
plot(White.mod2)
cookdist <- cooks.distance(White.mod2); cookdist
outlierTest(White.mod2)
hat(WhiteWine)
