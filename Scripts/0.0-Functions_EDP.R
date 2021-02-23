#-----------------------------------------------------------------------------#
#                                                                             
# Author:        Logan Stundal                                                    
# Date:          March 25, 2020                                                 
# Purpose:       Functions - this script collects all functions / themes used in 
#                            the Colombia Event Data Project.
#                                                                             
#
# Copyright (c): Logan Stundal, 2020                      
# Email:         stund005@umn.edu
#
#-----------------------------------------------------------------------------# 
#
# Notes:                                                                    
#                                                                             
#                                                                             
#-----------------------------------------------------------------------------#

require(ggplot2)

reg_table <- function(models       = NULL,
                      mod_names    = NULL,
                      var_names    = NULL,
                      robust       = FALSE, # For GLM, robust standard errors using sandwich
                      prec         = 3,
                      notes        = NULL, 
                      caption      = NULL, 
                      label        = NULL,
                      table_header = NULL,
                      se.size      = 'footnotesize',  # Can take any tex size: "tiny", "scriptsize", "small", etc...
                      kable_table  = FALSE){
  
  # NOTE: FOR NON-BAYESIAN TABLES: TAKES A DATAFRAME WITH COLUMNS: PARAM, SES, PVALS
  
  require(tidyverse)
  
  # ----------------------------------- #
  if(is.null(models) | !is.list(models)){
    stop('"models" is a required argument as a list of models or parameter data.frames.')
  }
  
  if(!any(c('inla','data.frame','glm') %in% unlist(sapply(models, class)))){
    stop('"models" only takes a list containing "inla", "glm", or "data.frame" objects in current version.')
  }
  # ----------------------------------- #
  
  
  # ----------------------------------- #
  # Verify appropriate model list
  coefs = models
  
  for(i in 1:length(coefs)){
    if('inla' %in% class(coefs[[i]])){
      coefs[[i]] = coefs[[i]]$summary.fixed[,c(1,3,5)]
    } else if('glm' %in% class(coefs[[i]])){
      if(robust == T){
        coefs.tmp = coef(coefs[[i]]) 
        se.tmp    = sqrt(diag(vcovHC(coefs[[i]], type = 'HC3')))
        p.tmp     = 2 * (1 - pnorm(abs(coefs.tmp / se.tmp)))
        
        coefs[[i]] = data.frame('point' = coefs.tmp,
                                'se'    = se.tmp,
                                'pval'  = p.tmp)
      } else{
        coefs[[i]] = summary(coefs[[i]])$coefficients[,c(1,2,4)]
        colnames(coefs[[i]]) = c('point', 'se', 'pval')
      }
    } else{
      coefs[[i]] = coefs[[i]]
      colnames(coefs[[i]]) = c('point', 'se', 'pval')
    }
  }  
  # ----------------------------------- #  
  
  
  # ----------------------------------- #
  # Prep dataframe for regression parameters
  if(is.null(mod_names)){
    mod_names = sprintf('Model.%s',1:length(coefs))
  }
  
  if(is.null(var_names)){
    var_names = lapply(coefs, function(x) rownames(x)) %>% unlist %>% unique 
    names(var_names) = var_names
  }
  
  {d = tibble('var_names' = rep('',(length(var_names)*2)))
    d$var_names[seq(1,nrow(d),2)] = as.character(var_names)
    d$var_names[seq(2,nrow(d),2)] = lapply(1:length(coefs), function(x){
      tmp.bayes = 'lcb' %in% colnames(coefs[[x]])
      tmp.val   = ifelse(tmp.bayes, 'HPD','SE')
      
      paste(as.character(var_names), tmp.val, sep = '.')
      
    }) %>% unlist %>% unique}
  # ----------------------------------- #
  
  
  # ----------------------------------- #
  # Tidy coefficient table for neat output
  coefs <- lapply(1:length(coefs), function(x){
    
    # format is inconsistent, rounding first resolve this.
    coefs[[x]] = round(coefs[[x]], prec)
    coefs[[x]] = format(x      = coefs[[x]],
                        format = 'f',
                        digits = prec)
    
    coefs[[x]] = as_tibble(coefs[[x]], rownames = NA)
    coefs[[x]] = add_column(coefs[[x]], 
                            'tmp_names' = rownames(coefs[[x]]))
    
    
    if('pval' %in% colnames(coefs[[x]])){
      coefs[[x]] = coefs[[x]] %>% 
        rename(Est.  = 1,
               SEs   = 2,
               pvals = 3) %>%
        mutate(var_names = as.character(unlist( var_names[coefs[[x]]$tmp_names] )),
               stars     = case_when(pvals <= 0.05 & pvals > 0.01   ~ "*", 
                                     pvals <= 0.01 & pvals > 0.001  ~ "**",
                                     pvals <= 0.001                 ~ "***",
                                     TRUE                           ~ "")) %>%
        mutate_if(is.character, str_trim) 
    } else{
      coefs[[x]] = coefs[[x]] %>% 
        mutate(SEs       = paste0('[',lcb,',',ucb,']'),
               var_names = as.character(unlist( var_names[coefs[[x]]$tmp_names] ))) %>%
        mutate_if(is.character, str_trim)
    }
  })
  # ----------------------------------- #
  
  
  # ----------------------------------- #
  # Construct parameter data.frame
  for(i in 1:length(coefs)){
    # Join parameter estimates
    tmp = sprintf('Model%s.est',i)
    
    if('stars' %in% colnames(coefs[[i]])){
      tmp_d = coefs[[i]][c('var_names','Est.','stars')] %>%
        mutate(tmp_par = paste0('$',`Est.`,sprintf('^{%s}',stars),'$')) %>%
        dplyr::select(var_names, tmp_par)
      d     = left_join(d,tmp_d, by = 'var_names') %>%
        rename(!!tmp := tmp_par) 
    } else{
      tmp_d = coefs[[i]][c('var_names','mean')] %>% 
        mutate(mean = sprintf('$%s$', mean)) %>%
        rename(!!tmp := 'mean')
      d     = left_join(d,tmp_d, by = 'var_names')
    }
    
    # Join standard errors
    tmp   = sprintf('Model%s.se',i)
    
    if('stars' %in% colnames(coefs[[i]])){
      tmp_d = coefs[[i]][c('var_names','SEs')] %>%
        mutate(SEs = sprintf('$(%s)$',SEs)) %>%
        rename(!!tmp := 'SEs') %>%
        mutate(var_names = paste0(var_names, '.SE'))
    } else{
      tmp_d = coefs[[i]][c('var_names','SEs')] %>% 
        mutate(SEs = sprintf('$%s$', SEs)) %>%
        rename(!!tmp := 'SEs') %>%
        mutate(var_names = paste0(var_names, '.HPD'))
    }
    
    d     = left_join(d,tmp_d, by = 'var_names')
    
    # Unite columns
    tmp = sprintf('Model.%s',i)
    d   = unite(d, !!tmp, (i+1):(i+2), remove = T, na.rm = T)  
  }
  
  # Final data.frame tidy
  d$var_names[seq(2,nrow(d),2)] = ''
  colnames(d)[2:(length(coefs)+1)] = mod_names
  # ----------------------------------- #
  
  
  # ----------------------------------- #
  # Build output - kable or tex
  if(kable_table == TRUE){
    return(knitr::kable(d))
  } else{
    
    tex = list()
    
    for(i in 1:nrow(d)){
      if(i %% 2 != 0){
        tex[i] = do.call(sprintf, c(list(strrep('%s & ',length(coefs)+1)), paste(d[i,]))) %>%
          str_trim(., side = 'both') %>%
          str_sub(., end = -2) %>%
          paste('\t\t\t\t',., '\\\\ \n')
      } else{
        tex[i] = do.call(sprintf, c(list(paste0('%s & ', strrep(paste0(sprintf('\\%s',se.size),'{%s} & '),
                                                                length(coefs)))), paste(d[i,]))) %>%
          str_trim(., side = 'both') %>%
          str_sub(., end = -2) %>%
          paste('\t\t\t\t\t',., '\\\\ \n') %>%
          str_replace_all(., c(sprintf('\\\\%s\\{\\}',se.size)),'')
      }
    }
  } 
  
  # FOR TEX, return formatted output
  tex  = unlist(tex)
  cols = length(coefs)
  
  tex_code = c(
    # Header information
    '\\begin{table}[!ht] \n',
    '\t\\begin{center} \n',
    sprintf('\t\t\\begin{tabular}{l%s} \n', strrep(' c',cols)),
    '\t\t\\hline \n',
    
    if(!is.null(table_header)){
      sprintf('\t\t\t& \\multicolumn{%s}{c}{%s} \\\\ \n',cols, table_header)
    },
    
    paste0('\t\t\t', do.call(sprintf, c(list(strrep('& %s ', cols)), mod_names)), '\\\\ \n'),
    '\t\t\t\\midrule \n',
    
    # Model coefficients here
    tex,
    
    # Footer information
    '\t\t\t\\hline \n',
    do.call(sprintf, c(list('\t\t\t\\multicolumn{%s}{l}{\\scriptsize{%s}} \n', cols+1, notes))),
    '\t\t\\end{tabular} \n',
    do.call(sprintf, c(list('\t\\caption{%s} \n', caption))),
    do.call(sprintf, c(list('\t\\label{table:%s} \n', label))),
    '\t\\end{center} \n',
    '\\end{table}'
    
  )
  cat(tex_code)
}


kappa_cm <- function(observed, 
                     pred   = NULL, 
                     p_bin  = NULL,
                     cutoff = 0.5, 
                     sig    = 0.95,
                     silent = TRUE){
  if(is.null(pred) & is.null(p_bin)){
    stop('Either "pred" or "p_bin" are required.')
  }
  
  if(is.null(p_bin)){
    p_bin = ifelse(pred >= cutoff, 1, 0) 
  }
  
  tab = table(observed, p_bin)
  
  p_agree  = (tab[1] + tab[4]) / sum(tab)
  p_random = (((tab[1] + tab[3]) / sum(tab)) * ((tab[1] + tab[2]) / sum(tab))) + 
    (((tab[2] + tab[4]) / sum(tab)) * ((tab[3] + tab[4]) / sum(tab)))
  
  k    = (p_agree - p_random) / (1 - p_random)
  se_k = sqrt(( p_agree * (1 - p_agree) ) / ( length(observed) * (1 - p_random)^2  )) 
  
  crit = qnorm(1 - (1-sig)/2)
  
  k_lci = k - crit*se_k
  k_uci = k + crit*se_k
  
  return(list('Kappa' = k,
              'Kappa_lci' = k_lci,
              'Kappa_uci' = k_uci))
  
  do.call(sprintf, c(list('Kappa: %s\n
                Kappa CI: [%s, %s]')),
          paste(round(c(k, k_lci, k_uci),3)))
  
  if(silent == FALSE){
    cat(sprintf('Kappa: %s\nKappa CI: [%s, %s]', round(k,3),round(k_lci,3),round(k_uci,3)))
  }
}

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


logan_theme <- {
  theme_minimal() +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title         = element_text(size = 12),
          plot.subtitle      = element_text(size = 10),
          plot.caption       = element_text(size = 10),
          axis.title         = element_text(size = 10),
          axis.text          = element_text(size = 10),
          legend.position    = 'bottom',
          legend.direction   = 'horizontal',
          legend.title       = element_blank())
}

map_theme <- {
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.direction   = 'horizontal',
        legend.title       = element_blank(),
        legend.key.width   = unit(0.8,'cm'),
        legend.key.height  = unit(0.8,'cm'))
}

#-----------------------------------------------------------------------------#
