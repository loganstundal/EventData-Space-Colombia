#-----------------------------------------------------------------------------#
#
# Author:        Logan Stundal
# Date:          February 27, 2021
# Purpose:       Supplemental functions to replicate analysis
#
#
# Copyright (c): Logan Stundal, 2021
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------#
#
# Notes:
#
#
#-----------------------------------------------------------------------------#

se_robust <- function(object,
                      robust = FALSE,
                      hc     = NULL){

  if(class(object) != 'SpatialProbit'){
    stop('Function only takes models of class: "SpatialProbit".')
  }
  mycoef    = object@coeff
  mod_covar = ifelse(object@varcov == "varcov", "UC", "UP")

  # Estimate vacriance covariance matrix
  # https://rdrr.io/cran/ProbitSpatial/src/R/lik_SEM_UC.R
  lik     = function (th, env){.Call(paste('lik',object@DGP,mod_covar, sep='_'), th, env, PACKAGE = "ProbitSpatial")}
  H       = numDeriv::hessian(lik, x = mycoef, env = object@env)
  se_vcov = abs(solve(H))

  # Estimate Standard Errors
  if(robust == TRUE & is.null(hc)){
    stop('Provide a value for correction type to "hc" argument: "HC0", "HC1", "HC2", or "HC3".')
  } else if(robust == TRUE){

    # All robust setup
    res      = as.numeric(object@y - object@X %*% object@beta)
    se_bread = (se_vcov / as.numeric(sum(res^2) / (object@nobs - object@nvar)))[1:object@nvar,1:object@nvar]
    X        = object@X
    se_hii   = diag(X %*% se_bread %*% t(X))

    se_rho   = sqrt(se_vcov[object@nvar + 1,object@nvar + 1])

    if(hc == 'HC0'){
      # HC)
      hc0 = se_bread %*% t(X) %*% diag(res^2) %*% X %*% se_bread
      se  = c(sqrt(diag(hc0)), se_rho)
    } else if(hc == 'HC1'){
      # HC1
      hc1 = as.numeric(object@nobs / (object@nobs - object@nvar)) * (se_bread %*% t(X) %*% diag(res^2) %*% X %*% se_bread)
      se  = c(sqrt(diag(hc1)), se_rho)
    } else if(hc == 'HC2'){
      # HC2
      hc2 = se_bread %*% t(X) %*% diag( (res^2 / (1 - se_hii)) ) %*% X %*% se_bread
      se  = c(sqrt(diag(hc2)), se_rho)
    } else{
      # HC3
      hc3 = se_bread %*% t(X) %*% diag( (res^2) / (1 - se_hii)^2 ) %*% X %*% se_bread
      se  = c(sqrt(diag(hc3)), se_rho)
    }
  } else{
    # Constant
    se      = sqrt(diag(se_vcov))
  }
  return(se)
}
#-----------------------------------------------------------------------------#

