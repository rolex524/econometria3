library(readstata13)
library(plm)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut3")
df <- read.dta13("Problem3, 4 e5.dta")

plm()
pggls()

#####

#http://www.polsci.ucsb.edu/faculty/glasgow/ps207/ps207_class1.r

## example of unobserved heterogeneity
x1 <- rnorm(100,1,1)
x2 <- rnorm(100,2,1)

e1 <- rnorm(100,0,0.8)
e2 <- rnorm(100,0,0.8)

y1 <- 0 - x1 + e1
y2 <- 5 - x2 + e2

x <- append(x1,x2)
y <- append(y1,y2)

plot(y~x)

reg.pooled <- lm(y~x)
reg.1 <- lm(y1~x1)
reg.2 <- lm(y2~x2)

abline(reg.pooled)
abline(reg.1, col="red")
abline(reg.2, col="red")

## Basic Panel Data models in R ##

pdim(df, index = c("ID","TIME"))

## Pooled model (regular OLS)

pooled.model <- lm(EARNINGS ~ UNION + MALE + EXP, data=df, na.action=na.omit)
summary(pooled.model)

## least square dummy variable (LSDV) model

lsdv.model <- lm(EARNINGS ~ UNION + MALE + EXP + as.factor(TIME), data=df, na.action=na.omit)
summary(lsdv.model)

lsdv.model <- lm(EARNINGS ~ UNION + MALE + EXP + as.factor(ID), data=df, na.action=na.omit)
summary(lsdv.model)

## F test for fixed effects

anova(pooled.model, lsdv.model)

## Using the plm package ##

pmodel1 <- plm(EARNINGS ~ UNION + MALE + EXP, data=df, index=c("ID","TIME"), na.action=na.omit, model="pooling")
summary(pmodel1)

## Within model

pmodel2 <- plm(EARNINGS ~ UNION + MALE + EXP, data=df, index=c("ID","TIME"), na.action=na.omit, model="within")
summary(pmodel2)

## F test for fixed effects

pFtest(pmodel2,pmodel1)

## Random effects model

pmodel3 <- plm(EARNINGS ~ UNION + MALE + EXP, data=df, index=c("ID","TIME"), na.action=na.omit, model="random")
summary(pmodel3)

## Breusch-Pagan test 

plmtest(pmodel3, effect="individual", type="bp")

## Hausman test

phtest(pmodel2, pmodel3)

## First difference model -- not often used in political science panel data
## Differences from the within estimator usually indicate a violation of the strict exogeneity assumption

pmodel.fd <- plm(EARNINGS ~ UNION + MALE + EXP, data=df, index=c("ID","TIME"), na.action=na.omit, model="fd")
summary(pmodel.fd)

## FGLS
gls.model <- pggls(EARNINGS ~ UNION + MALE + EXP, data=df, index=c("ID","TIME"),na.action=na.omit, model="within")
summary(gls.model)

#####
pdata <- pdata.frame(df, index = c("TIME","ID"))
summary(
  plm(EARNINGS ~ UNION + MALE + EXP, data=df,index=c("ID","TIME"),na.action=na.omit, model="random")
)
 