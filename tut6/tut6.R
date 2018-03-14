library(readstata13)
library(plm)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut6")

df <- read.dta13("Problem6.dta")

avg.educ <- (df$educ + df$educt)/2
equations <- list(
  eq1 = lwage ~ educ + avg.educ + age + white + female,
  eq1 = lwage ~ educ + avg.educ + age + white + female + uncov + married + tenure
)

lapply(equations, lm, data = df)

lapply(equations, plm, data = df)

## Using the plm package ##
# nao achei o ID da familia na data, tem q substituir onde ta "?"
pmodel1 <- plm(equations$eq1, data=df, index=c("?","first"), na.action=na.omit, model="pooling")
pmodel1 <- pggls(equations$eq1, data=df, index=c("?","first"), na.action=na.omit, model="pooling")
summary(pmodel1)

## Within model

pmodel2 <- plm(equations$eq1, data=df, index=c("?","first"), na.action=na.omit, model="within")
pmodel2.gls <- pggls(equations$eq1, data=df, index=c("?","first"), na.action=na.omit, model="within")
summary(pmodel2)

## F test for fixed effects

pFtest(pmodel2,pmodel1)