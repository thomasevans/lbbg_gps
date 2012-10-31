#'a place to store useful little code snippets

#'get a list of available devices:
devices <- sort(unique(gps$device_info_serial))



#'Parallel function foreach is useful for where vectorised function is not possible.
require(foreach)
require(doParallel)
cl <- makeCluster(parallel::detectCores())     #use x cores, general solution for any windows machine.
registerDoParallel(cl)   #start the parellel session of R; the 'slaves', which will run the analysis.
clusterExport(cl, c("x"))   #export vairables that will be needed in the 'slaves'.

#mast a list object to recieve the data
lst <- list()

#close cluster
stopCluster(cl)