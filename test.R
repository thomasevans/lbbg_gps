
library(xtable)
print(xtable(xtable(vec.all), type="html", file=paste0("flight_",flight,".html")))
# vec.all
#useing anser from: http://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
# rm(list=setdiff(ls(), c("i","flights.sample")))
# }
# rm(list=ls())
# warnings()
plot(vec.all$angle_dif ~ points$nest_gc_dist, xlim = c(max(points$nest_gc_dist)+2,min(points$nest_gc_dist)-1))