library(readstata13)
library(plm)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut7")
df <- read.dta13("Problem7.dta")


eq <- list(
  eq1 = criv ~ polpc
)

#FD Model
pmodel.fd <- plm(criv ~ polpc, data=df, index=c("state","year"), na.action=na.omit, model="fd")
summary(pmodel.fd)

## FGLS
gls.model <- pggls(criv ~ polpc, data=df, index=c("state","year"),na.action=na.omit, model="within")
summary(gls.model)

## Within model

pmodel2 <- plm(criv ~ polpc, data=df, index=c("state","year"), na.action=na.omit, model="within")
summary(pmodel2)

## F test for fixed effects

pFtest(pmodel2,pmodel1)

## Random effects model

pmodel3 <- plm(criv ~ polpc, data=df, index=c("state","year"), na.action=na.omit, model="random")
summary(pmodel3)

## Breusch-Pagan test 

plmtest(pmodel3, effect="individual", type="bp")

## Hausman test

phtest(pmodel2, pmodel3)