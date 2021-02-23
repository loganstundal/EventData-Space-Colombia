
rm(list=ls())
library(ProbitSpatial)
library(ggplot2)
source('c:/users/logan/googledrive/umn/research/RA_John/event_data_project/scripts/functions_edp.r')

n <- 1000
nneigh <- 3
rho <- 0.5
beta <- c(4,-2,1)
W <- generate_W(n,nneigh,seed=123)
X <- cbind(1,rnorm(n,2,2),rnorm(n,0,1))
colnames(X) <- c("intercept","X1","X2")
y <- sim_binomial_probit(W=W,X=X,beta=beta,rho=rho,model="SEM")

y2 <- rbinom(n,1,.5)
d <- as.data.frame(cbind(y,y2,X))


mod <- SpatialProbitFit(y2~X1+X2,d,W,
                        DGP='SEM',method="conditional",varcov="varcov")

mod2 <- SpatialProbitFit(y2~X1+X2,d,W,
                        DGP='SEM',method="full-lik",varcov="varcov")



summary(mod,  covar = T)
summary(mod2, covar = T)


summary(glm(y2 ~ X1 + X2, data = d, family = binomial(link = 'probit')))




z <- se_robust(mod, robust = T, hc = 'HC3')


b = mod@coeff
tval = abs(mod@coeff / z)
pval = 1 - pt(tval, df = mod@nobs - mod@nvar)
star = dplyr::case_when(pval < 0.1   & pval > 0.05  ~ '.',
                        pval < 0.05  & pval > 0.01  ~ '*',
                        pval < 0.01  & pval > 0.001 ~ '**',
                        pval < 0.001                ~ '***',
                        TRUE ~ '')
s
cbind(round(data.frame('Est.'  = b,
                       'se'    = z,
                       'T.Val' = tval,
                       'P'     = pval),3),star)
summary(mod, covar = T)
