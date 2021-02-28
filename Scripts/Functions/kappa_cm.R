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

kappa_cm <- function(observed,
                     pred   = NULL,
                     p_bin  = NULL,
                     cutoff = 0.5,
                     sig    = 0.95){
  # Cohen's Kappa
  # Kappa = 1 at perfect agreement, 0 (or negative) at no agreement or
  # agreement worse than random chance.

  # observed - true values
  # p_bin    - Reported values
  # pred     - fn can take predicted probabilities to convert to binary
  #            outcomes (if pred, fn uses "cutoff" to define binary vals.)
  # cutoff   - value above which a predicted probability is coded as an event
  # sig      - significance values for reported confidence intervals

  if(is.null(p_bin)){
    p_bin <- ifelse(pred >= cutoff, 1, 0)
  }

  tab <- table(observed, p_bin)

  p_agree  = (tab[1] + tab[4]) / sum(tab)
  p_random = (((tab[1] + tab[3]) / sum(tab)) * ((tab[1] + tab[2]) / sum(tab))) +
    (((tab[2] + tab[4]) / sum(tab)) * ((tab[3] + tab[4]) / sum(tab)))

  k    = (p_agree - p_random) / (1 - p_random)
  se_k = sqrt(( p_agree * (1 - p_agree) ) / ( length(observed) * (1 - p_random)^2  ))

  crit = qnorm(1 - (1-sig)/2)

  k_lci = k - crit*se_k
  k_uci = k + crit*se_k

  return(data.frame('Kappa_lci' = k_lci,
                    'Kappa'     = k,
                    'Kappa_uci' = k_uci))
}
#-----------------------------------------------------------------------------#
