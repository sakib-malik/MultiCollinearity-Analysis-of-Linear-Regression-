## Load the Data
gas <- read.csv("https://www.dropbox.com/s/xhcep7n4tpm0exg/gasoline.csv?dl=1")  ## from share able link dropbox

## Removing the missing observations from the data
gas1 <- na.omit(gas)

## Creating a new column PCIncome = INCOME/POP
gas1$PCINCOME <- gas1$INCOME/gas1$POP

## Removing the outliers
## For "PCIncome"
PCINCOME.out <- boxplot(gas1$PCINCOME,plot=FALSE)$out
## For "GasP"
GASP.out <- boxplot(gas1$GASP,plot=FALSE)$out
## For "PD"
PD.out <- boxplot(gas1$PD,plot=FALSE)$out
## For "PN"
PN.out <- boxplot(gas1$PN,plot=FALSE)$out
## For "PS"
PS.out <- boxplot(gas1$PS,plot=FALSE)$out

## No Outliers found so no need to remove them

## X1 contains constant, GasP, PCIncome 
X1 <- cbind(1, gas1[, c(4, 12)])
names_X1 <- names(X1)
names_X1[1] = "INTERCEPT"
X1 <- as.matrix(X1)

## X2 contains PD, PN, PS
X2 <- gas[, 9:11]
names_X2 <- names(X2)
X2 <- as.matrix(X2)

## Regress each variable of X2 on X1
Mod1 <- lm(X2[, 1] ~ X1 - 1)   ## we already have intercept term in X1
Mod2 <- lm(X2[, 2] ~ X1 - 1)
Mod3 <- lm(X2[, 3] ~ X1 - 1)

## Z2 contains residuals from each regression
Z2 <- cbind(resid(Mod1), resid(Mod2), resid(Mod3))
names_Z2 <- c("Ed", "En", "Es")

## regress y = GASEXP on X1 and X2 and then X1 and Z2
Mod4 <- lm(as.matrix(gas$GASEXP) ~ X1 + X2 - 1)  ## we already have intercept term in X1
Mod5 <- lm(as.matrix(gas$GASEXP) ~ X1 + Z2 - 1)
names(Mod4$coefficients) <- c(names_X1,names_X2)
names(Mod5$coefficients) <- c(names_X1,names_Z2)

## Comparing the outputs
library(stargazer)
stargazer(Mod4, Mod5, title = "Models", column.labels = c("X1-X2", "X1-Z2"), type = "text")

#####  ----   CONCLUSION ---- ######
# Coefficients for (PD, PN, PS) is same as that of residuals(Z2) i.e Error_Pd, Error_Pn, Error_Ps

