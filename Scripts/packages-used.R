
library(stringr)
library(purrr)
x <- sapply(c("cowplot","dplyr","forcats","ggplot2","ggrepel","kableExtra",
         "ProbitSpatial","pROC","purrr","raster","sandwich","scales",
         "sf","spdep","stringr","tibble","tidyr"), function(x){
           paste(x, packageVersion(x), sep = " - ")}) %>%
  bind_rows() %>% t %>% as.data.frame %>%
  remove_rownames() %>% arrange(V1) %>%

  rename(package = 1) %>%
  mutate(version = str_split(package, pattern = " - ", n = 2) %>%
           map(2) %>% unlist) %>%
  mutate(package = str_split(package, pattern = " - ", n = 2) %>%
           map(1) %>% unlist)


noquote(paste(x$package, collapse = '", "'))
noquote(paste(x$version, collapse = '", "'))
