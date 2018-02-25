library(systemfit)
library(readstata13)
library(dplyr)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut2")
df <- read.dta13("Problem2.dta")

eq1 <- hours ~ lwage + educ + age + kidslt6 + kidsge6 + nwifeinc
eq2 <- lwage ~ hours + educ + exper + expersq
instrument1 <- ~ age + kidslt6 + kidsge6 + nwifeinc+ exper + expersq
lm(eq1, df)
lm(eq2, df)

systemfit(
  formula = list(eq1,eq2), 
  data = df) %>% 
  summary()

systemfit(
  formula = list(supply = eq1, demand = eq2), 
  method = "2SLS", 
  inst = instrument1,
  data = df) %>% 
  summary()
