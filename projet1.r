# install.packages("ggplot2",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
harmonic <- function (xs) length(xs)/sum(1/xs[xs>0],na.rm=T)
set.seed(334)
highwayLength <- 1000
maxCars <- 400
lanes <- 3
lastLane <- lanes
gencars <- function (maxCars,lane) {
    x <- sort(unique(trunc(runif(maxCars, 1, highwayLength))))
    aggressive <- x%%20==0
    data.frame(x,lane,speed=0,aggressive)
}
highway <- lapply(letters[1:lanes], function (lane) gencars(maxCars/ifelse(lane==1||lane==lastLane,2,1),lane))
png("test.png",width=1920)
?png
ggplot(mapping=aes(x=x,y=lane,col=aggressive)) +
    geom_point(data=highway[[1]]) +
    geom_point(data=highway[[2]]) +
    geom_point(data=highway[[3]]) +
    scale_color_manual(values=c("black","red"))
# ggplot(highway[[2]],aes(x=x,y=y)) + geom_point()
dev.off()
system("xdg-open test.png")

cars <- data.frame(a=gencars(1000),b=gencars(1000),c=gencars(1000))
cars

apply(cars,2,function (x) c(x,T))

cars$aggressive <- cars$x%%20==0
cars

cars <- apply(cars,1,function (car) c(aggressive=car%%20==0,position=car))
unname(cars)
# cars
data.frame(cars)

data <- trunc(abs(rnorm(10)*10)+3)
xs <- c(1,4,16)
xs
sapply(c(mean,geom,harmonic),function (f) f(xs))
ys <- c(2,4,8)
ys
sapply(c(mean,geom,harmonic),function (f) f(ys))
data
sapply(c(mean,geom,harmonic),function (f) f(data))
