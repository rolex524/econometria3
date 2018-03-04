library(systemfit)
library(readstata13)
library(dplyr)
library(stargazer)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut2")
df <- read.dta13("Problem2.dta")
df <- df[df$hours != 0,]

eq1 <- hours ~ lwage + educ + age + kidslt6 + kidsge6 + nwifeinc
eq2 <- lwage ~ hours + educ + exper + expersq
instrument1 <- ~ age + kidslt6 + kidsge6 + nwifeinc+ exper + expersq
lm(eq1, df)
lm(eq2, df)

stargazer(lm(eq1, df), lm(eq2, df),type = "html", out = "C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut2/output1.html")

systemfit(
  formula = list(eq1,eq2), 
  data = df) %>% 
  #summary() %>%

systemfit(
  formula = list(supply = eq1, demand = eq2), 
  method = "2SLS", 
  inst = instrument1,
  data = df) %>% 
  summary()

#2sls na mão
#jeito 1 ####
#regride as var dependentes nas variaveis exogenas e achamos
#uma combinação linear das variaveis exogenas que será um instrumento (Z)
fit_1.1 <- lm(hours ~ age + kidslt6 + kidsge6 + nwifeinc, df) %>% fitted()
fit_2.1 <- lm(lwage ~ exper + expersq, df) %>% fitted()

fit_1.2 <- lm(fit_1.1 ~ lwage + educ, df)
fit_2.2 <- lm(fit_2.1 ~ hours + educ, df)

#jeito 2 agora vai####
#http://www.rpubs.com/cyobero/simultaneous-models
#regride cada uma das equações na sua forma reduzida
#e depois regride as equações originais substituindo as var endogenas pelas suas versões estimada a cima
hours_redu <- lm(hours ~ age + kidslt6 + kidsge6 + nwifeinc+ exper + expersq, df) %>% fitted()
lwage_redu <- lm(lwage ~ age + kidslt6 + kidsge6 + nwifeinc+ exper + expersq, df) %>% fitted()

fit_1.2 <- lm(hours ~ lwage_redu + educ + age + kidslt6 + kidsge6 + nwifeinc, df)
fit_2.2 <- lm(lwage ~ hours_redu + educ + exper + expersq, df)

stargazer(fit_1.2,fit_2.2, type = "html", out = "C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut2/output2.html")
