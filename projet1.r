# install.packages("ggplot2",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
harmonic <- function (xs) length(xs)/sum(1/xs[xs>0],na.rm=T)
set.seed(334)
highwayLength <- 5000
maxCars <- 200
lanes <- 3
lastLane <- lanes
gencars <- function (maxCars,lane) {
    x <- sort(unique(trunc(runif(maxCars, 1, highwayLength))))
    aggressive <- x%%20==0
    data.frame(x,lane,speed=0,aggressive)
}
highway <- lapply(letters[1:lanes], function (lane) gencars(maxCars/ifelse(lane==1||lane==lastLane,2,1),lane))
pdf("test.pdf",width=12)
ggplot(mapping=aes(x=x,y=lane,col=aggressive)) +
    geom_point(data=highway[[1]]) +
    geom_point(data=highway[[2]]) +
    geom_point(data=highway[[3]]) +
    scale_color_manual(values=c("black","red"))
dev.off()
system("explorer test.pdf")
