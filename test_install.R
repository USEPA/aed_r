library(foreach)
library(doSNOW)
library(rbenchmark)

cluster <- makeCluster(4,type="SOCK")
registerDoSNOW(cluster)
benchmark(
  foreach(n = 1:4) %do% install.packages("formatR",repos = "https://cran.rstudio.com/"),
  foreach(n = 1:4) %dopar% install.packages("formatR",repos = "https://cran.rstudio.com/"),
  replications = 4
)  



stopCluster(cluster)
