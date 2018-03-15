# install.packages("zeallot",'.',dep=T,repos="http://cran.stat.sfu.ca")
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
v.vmax <- function (vs) ifelse(vs == truck, 29, ifelse(vs == moto, 50, 32.5)) #faster moto
v.safedistance <- function (vs) ifelse(vs == truck, 30, 10)#2sec*speed ?!? or 150ish brake distance/truck
hw <- list(lanes = 3, vehicles = 200, length = 16000, change.period = 20)
v.random <- function (n, lane) {
    against <- if (lane == hw$lane) 0.75 else 1
    carratio <- 0.73/against
    truckratio <- if (lane == hw$lane) 0 else 0.25/against
    truckratio <- if (lane == hw$lane) 0 else 0.15/against
    vehicles <- rbinom(n, 1, carratio)
    factor(ifelse(vehicles, vehicles, rbinom(n, 1, truckratio) + 2), levels)
}
lane.random <- function (lane) {
    n <- hw$vehicles/ifelse(lane==1||lane==hw$lanes,2,1)
    position <- (1:n)*(hw$length/n)
    # position <- sort(unique(trunc(runif(2*n, 1, hw$length)))[1:n])
    type <- v.random(n, lane)
    speed <- (rbeta(n,2,0.5)/2+0.25)*v.vmax(type)
    data.frame(position, type, lane, original = lane, speed, crashed=F, last.lanechange=0)
}
highway <- lapply(seq(hw$lane), lane.random)

stepby <- 4
duration <- stepby*60
run <- function (highway, rules) {
    allcars <- list()
    i <- 1
    time <- 0
    repeat {
        if (time %% stepby == 0) {
            allcars[[i]] <- rbindlist(highway)
            i <- i+1
            if (time >= duration)
                break;
        }
        highway <- rules(highway)
        time <- time+1
    }
    data.frame(rbindlist(allcars))
}

v.nose <- function (lane) lane$position + v.length(lane$type)
v.safe <- function (lane) v.nose(lane) + v.safedistance(lane$type)
maxaccel <- function (v1s, v2s) v2s$position + v2s$speed - (v.safe(v1s) + v1s$speed)

checkbroken <- function (text, lane, v=NULL) {
    h <- head(lane,-1)
    broken <- v.nose(h) > tail(lane,-1)$position
    if (any(!h$crashed & broken)) {
        where <- which(broken)
        print(c(text, where, v))
        indexes <- row(lane)[,1] <= where[1]+1
        print(lane[indexes,])
        stop()
    }
}

for (i in seq(highway)) {
    hwi <- highway[[i]]
    checkbroken(sprintf("initial-%i",i), hwi)
}

changelane <- function (highway, o, t, candidates) {
    origin <- highway[[o]]
    target <- highway[[o+t]]
    tokeep <- rep(T, nrow(origin))
    candidates <- candidates & origin$last.lanechange == 0
    if (any(candidates)) {
        checkbroken("broken-before", target)
        for (j in which(candidates)) {
            v <- origin[j,]
            safe.ahead <- c(target$position > v.safe(v), T)
            safe.behind <- c(T, v$position > v.safe(target))
            can.accelerate <- c(maxaccel(v, target) > 0, T)
            candidate <- safe.ahead & safe.behind & can.accelerate
            if (any(candidate)) {
                v$lane <- v$lane + t
                v$last.lanechange <- hw$change.period
                behind <- seq(nrow(target)) < which(candidate)
                target <- rbindlist(list(target[behind,], v, target[!behind,]))
                tokeep[j] <- F
                checkbroken("broken-after", target, v)
            }
        }
        origin <- origin[tokeep,]
        rownames(origin) <- NULL
    }
    list(origin = origin, target = target, unchanged = tokeep)
}

rules.basic <- function (highway) {
    virtual.ahead <- list(position=.Machine$integer.max,type=car,lane=0,original=0,speed=0,crashed=F,last.lanechange=0)
    for (i in seq(highway)) {
        lane <- highway[[i]]
        n <- nrow(lane)
        t <- rbind(tail(lane,-1),virtual.ahead)
        accel <- ifelse(lane$crashed, 0, pmin(v.vmax(lane$type) - lane$speed, v.accel(lane$type)))
        safeaccel <- pmin(maxaccel(lane, t), accel)
        if (i < hw$lane) {
            slowed <- !lane$crashed & safeaccel < accel & safeaccel <= 0
            result <- changelane(highway, i, 1, slowed)
            lane <- result$origin
            highway[[i+1]] <- result$target
            safeaccel <- safeaccel[result$unchanged]
        }
        lane$speed <- pmax(lane$speed + safeaccel,0)
        lane$position <- lane$position + lane$speed
        lane$lane <- lane$lane + 1/duration
        lane$last.lanechange <- pmax(0, lane$last.lanechange-1)
        crashed <- v.nose(head(lane, -1)) > tail(lane, -1)$position
        lane$crashed <- lane$crashed | c(crashed,F) | c(F,crashed)
        lane$speed <- ifelse(lane$crashed, 0, lane$speed)
        highway[[i]] <- lane
    }
    for (i in hw$lanes:3) {
        lane <- highway[[i]]
        cruising <- lane$speed == v.vmax(lane$type) | lane$speed == 0
        result <- changelane(highway, i, -1, cruising)
        highway[[i]] <- result$origin
        highway[[i-1]] <- result$target
    }
    highway
}

highways <- run(highway, rules.basic)
# print(highways)

pdf("test.pdf",width=12)
map <- aes(x=position, y=lane, size=type, color=type)
ggplot() +
    scale_size_manual(values=c(0.1,0.1,0.1),labels=c("Car","Moto","Truck")) +
    scale_color_manual(values=c("Black", "Red","Blue")) +
    geom_point(map, highways, show.legend=F)
dev.off()
system("xdg-open test.pdf")
# system("explorer test.pdf")
