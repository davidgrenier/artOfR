# install.packages("ggplot2",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
library(zeallot)
library(data.table)
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
set.seed(334)
levels <- factor(1:3)
c(car,moto,truck) %<-% levels
v.length <- function (vs) ifelse(vs == moto, 2.2, ifelse(vs == car, 4.5, 14.6))
v.mass <- function (vs) ifelse(vs == moto, 175, ifelse(vs == car, 1850, 9750))
v.accel <- function (vs) ifelse(vs == moto, 7, ifelse(vs == car, 3, 0.6))
v.vmax <- function (vs) ifelse(vs == truck, 29, 32.5)
v.random <- function (n) {
    vehicles <- rbinom(n,1,0.73)
    factor(ifelse(vehicles, vehicles, rbinom(n,1,0.25/0.27)+2),levels)
}
hw <- list(lanes = 3, vehicles = 200, length = 120000)
lane.random <- function (lane) {
    n <- hw$vehicles/ifelse(lane==1||lane==hw$lanes,2,1)
    position <- sort(unique(trunc(runif(hw$length, 1, hw$length)))[1:n])
    v.type <- v.random(n)
    speed <- (rbeta(n,2,0.5)/2+0.5)*v.vmax(v.type)
    data.frame(position,v.type,lane,speed)
}
highway <- lapply(seq(hw$lane), lane.random)

stepby <- 36
duration <- 3600
run <- function (highway, rules) {
    highways <- list()
    i <- 1
    time <- 0
    repeat {
        if (time %% stepby == 0) {
            highways[[i]] <- rbindlist(highway)
            i <- i+1
            if (time >= duration)
                break;
        }
        highway <- rules(highway)
        time <- time+1
    }
    data.frame(rbindlist(highways))
}

rules.basic <- function (highway) {
    safety <- 5
    cartoofar <- list(position=9999999,v.type=car,lane=1,speed=0)
    for (i in seq(highway)) {
        lane <- highway[[i]]
        t <- tail(lane, -1)
        t <- rbind(t,cartoofar)
        saferoom <- t$position + t$speed - (lane$position + v.length(lane$v.type) + safety + lane$speed)
        accel <- pmin(saferoom, lane$position + v.length(lane$v.type), v.vmax(lane$v.type)-lane$speed, v.accel(lane$v.type))
        lane$speed <- lane$speed + accel
        lane$position <- (lane$position + lane$speed) %% hw$length
        lane$lane <- lane$lane + 1/(stepby*100)
        highway[[i]] <- lane
    }
    highway
}

highways <- run(highway, rules.basic)

pdf("test.pdf",width=12)
map <- aes(x=position, y=lane, size=v.type, color=v.type)
ggplot() +
    scale_size_manual(values=c(0.1,0.1,0.8),labels=c("Car","Moto","Truck")) +
    scale_color_manual(values=c("Black", "Red","Black")) +
    geom_point(map, highways, show.legend=F)
dev.off()
system("xdg-open test.pdf")
