# install.packages("ggplot2",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
library(zeallot)
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
set.seed(334)
levels <- 1:3
c(car,moto,truck) %<-% factor(levels)
len <- function (vehicles) ifelse(vehicles == moto, 2.2, ifelse(vehicles == car, 4.5, 14.6))
mass <- function (vehicles) ifelse(vehicles == moto, 175, ifelse(vehicles == car, 1850, 9750))
accel <- function (vehicles) ifelse(vehicles == moto, 7, ifelse(vehicles == car, 3, 0.6))
vmax <- function (vehicles) ifelse(vehicles == truck, 29, 32.5)
vehicles.random <- function (n) {
    vehicles <- rbinom(n,1,0.73)
    factor(ifelse(vehicles, vehicles, rbinom(n,1,0.25/0.27)+2),levels)
}
hw <- list(lanes = 3, vehicles = 200, length = 5000)
lane.random <- function (lane) {
    n <- hw$vehicles/ifelse(lane==1||lane==hw$lanes,2,1)
    position <- sort(unique(trunc(runif(hw$length, 1, hw$length)))[1:n])
    car.type <- vehicles.random(n)
    speed <- (rbeta(n,2,0.5)/2+0.5)*vmax(car.type)
    data.frame(position,car.type,lane,speed)
}
highway <- lapply(1:hw$lane, lane.random)
# lane1 <- highway[[1]]
# lane1$position <- (lane1$position + vmax(lane1$car.type)) %% hw$length
# lane1

# duration <- 3600
# stepby <- 36

# step <- function (time, rules, highways, highway) {
#     if (time > duration)
#         rbind(highways, highway)
#     else if (time %% stepby == 0)
#         step(time+1,rbind(highways,highway),highway)
#     else
# }

highways <- data.frame()
i <- 0
repeat {
    if (i >= 100)
        break;
    for (lane in highway) {
        lane$position <- (lane$position + i*lane$speed) %% 5000
        lane$lane <- lane$lane + i/100
        highways <- rbind(highways,lane)
    }
    i <- i+1
}
pdf("test.pdf",width=12)
map <- aes_(
    x=~position,
    y=~lane,
    size=quote(car.type)
)
ggplot() +
    scale_size_manual(values=c(0.1,0.5,2),labels=c("Moto","Car","Truck")) +
    geom_point(map, highways, show.legend=F)
dev.off()
system("xdg-open test.pdf")
