

llik <- function(simple, complex, df){
  test_stat <- -2 * (as.numeric(simple) - as.numeric(complex))

  pval <- pchisq(test_stat, df = df, lower.tail = FALSE)

  msg <- sprintf(paste0("---------------------------\n",
                        "Test statistic: %s\n",
                        "---------------------------\n",
                        "Pvalue: %s\n",
                        "---------------------------\n"),
                 round(test_stat, 3),
                 format(round(pval,3), nsmall = 3))

  cat(msg)
}

llik(simple = -445.257, complex = -436.898, df = 1)
llik(simple = -456.720, complex = -431.704, df = 1)
llik(simple = -585.001, complex = -494.029, df = 1)

llik(simple = -515.653, complex = -467.298, df = 1)
llik(simple = -470.937, complex = -443.376, df = 1)
