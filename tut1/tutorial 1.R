install.packages("readstata13")
install.packages("systemfit")
library(readstata13)
library(systemfit)
library(dplyr)
library(RFGLS)

setwd("C:\\Users\\tadeu\\Desktop\\FGV\\semestre 7\\econometria 3\\tutoriais\\tut1")

df <- read.dta13("Problem1.dta")

#sindicalizado
df$union

#beneficios e salario
equations <- list(
  eq1 = hrearn ~ union,
  eq2 = pension ~ union,
  eq3 = insur ~ union,
  eq4 = hrbens ~ union
)

#calculando por ols
summary(systemfit(equations, data = df, method = "OLS"))
l <- lapply(equations, lm, data = df) %>% lapply(summary)

#achando a matriz de covariancia dos residuos
sigma_hat <- lapply(equations, lm, data = df) %>% 
  lapply(residuals) %>% 
  as.data.frame() %>%
  cov(res)

#achando o novo beta chapeu
x <- cbind(df$union,df$union,df$union,df$union)
y <- cbind(df$hrearn, df$pension, df$insur, df$hrbens)
beta_hat <- solve(t(x)%*%x)%*%t(x)%*%y


summary(systemfit(equations, data = df, method = "SUR"))













fgls(df$hrearn ~ df$union)

sigma_hat <- function(eq, df){
  reg <- lm(eq, df)
  res <- residuals(reg)
  sigma_hat <- length(res)^(-1) * res %*% res
  return(sigma_hat)
}

sigma_hat(hrearn ~ union + pension, df)
sigmaHat(lm(eq1, df))

beta_hat <- 