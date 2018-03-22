# install.packages("ggthemes",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
library(zeallot)
library(data.table)
library(ggthemes)
# system.time({
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
set.seed(335)
levels <- factor(1:3)
c(car,moto,truck) %<-% levels
v.length <- function (vs) ifelse(vs == moto, 2.2, ifelse(vs == car, 4.5, 14.6))
v.mass <- function (vs) ifelse(vs == moto, 175, ifelse(vs == car, 1850, 9750))
v.accel <- function (vs) ifelse(vs == moto, 7, ifelse(vs == car, 3, 0.6))
v.vmax <- function (vs) ifelse(vs == car, 30, ifelse(vs == truck, 27, 33))
x.second.rule <- 2
hw <- list(lanes = 3, vehicles = 200, length = 8000, change.period = 10)
v.random <- function (n, lane) {
    against <- if (lane == hw$lane) 0.75 else 1
    carratio <- 0.73/against
    truckratio <- if (lane == hw$lane) 0 else 0.92
    vehicles <- rbinom(n, 1, carratio)
    factor(ifelse(vehicles, vehicles, rbinom(n, 1, truckratio) + 2), levels)
}
lane.random <- function (lane) {
    n <- hw$vehicles*ifelse(lane==1||lane==hw$lanes,0.5,1)
    position <- (1:n)*(hw$length/n)
    type <- v.random(n, lane)
    speed <- (rbeta(n,2,0.5)/2+0.25)*v.vmax(type)
    data.table(position, type, lane = lane-0.5, speed, crashed=F, last.lanechange=0)
}
highway <- lapply(seq(hw$lane), lane.random)

stepby <- 1
duration <- stepby*60
run <- function (highway, rules) {
    overflow <- rep(0, hw$lanes)
    allcars <- list()
    i <- 1
    time <- 0
    repeat {
        if (time %% stepby == 0) {
            if (time >= duration)
                break;
            allcars[[i]] <- rbindlist(highway)
            i <- i+1
        }
        result <- rules(highway, overflow)
        highway <- result$highway
        overflow <- result$overflow
        time <- time+1
    }
    print(sum(sapply(highway, function (lane) lane[,sum(crashed)])))
    print(overflow)
    data.frame(rbindlist(allcars))
}

v.nose <- function (lane) lane$position + v.length(lane$type)
v.safe <- function (lane) v.nose(lane) + x.second.rule*lane$speed
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

virtual.tofront <- function (lane) {
    v <- lane[1,]
    v$position <- v$position + hw$length
    v
}

virtual.toback <- function (lane) {
    v <- tail(lane,1)
    v$position <- v$position - hw$length
    v
}

changelane <- function (highway, i, t, candidates) {
    origin <- highway[[i]]
    target <- highway[[i+t]]
    tokeep <- rep(T, nrow(origin))
    candidates <- candidates & origin$last.lanechange == 0 & (i+t < hw$lanes | origin$type != truck)
    if (any(candidates)) {
        checkbroken("broken-before", target)
        for (j in which(candidates)) {
            v <- origin[j,]
            safe.ahead <- c(target$position, virtual.tofront(target)$position) > v.safe(v)
            behind <- rbindlist(list(virtual.toback(target), target))
            safe.behind <- v$position > v.safe(behind)
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

rules.basic <- function (highway, overflow) {
    for (i in seq(highway)) {
        lane <- highway[[i]]
        n <- nrow(lane)
        t <- rbindlist(list(lane[-1,], virtual.tofront(lane)))

        accel <- ifelse(lane$crashed, 0, pmin(v.vmax(lane$type) - lane$speed, v.accel(lane$type)))
        safeaccel <- pmin(maxaccel(lane, t), accel)
        if (i < hw$lane) {
            slowed <- !lane$crashed & safeaccel < accel & safeaccel <= 0
            result <- changelane(highway, i, 1, slowed)
            lane <- result$origin
            highway[[i+1]] <- result$target
            safeaccel <- safeaccel[result$unchanged]
        }

        lane[,speed := pmax(speed + safeaccel, 0)]
        lane[,position := position + speed]
        lane[,lane := lane + 1/duration]
        lane[,last.lanechange := pmax(0, last.lanechange - 1)]
        ahead <- rbindlist(list(lane[-1,], virtual.tofront(lane)))
        crashed <- v.nose(lane) > ahead$position
        lane$crashed <- lane$crashed | crashed | c(tail(crashed, 1), head(crashed, -1))
        lane[, speed := ifelse(crashed, 0, speed)]
        newposition <- lane$position %% hw$length
        overflow[i] <- overflow[i] + sum(lane$position != newposition)
        lane[, position := newposition]
        neworder <- order(lane$position)
        lane <- lane[neworder]
        highway[[i]] <- lane
    }
    for (i in hw$lanes:3) {
        lane <- highway[[i]]
        cruising <- lane[, speed == 0 | speed == v.vmax(type)]
        result <- changelane(highway, i, -1, cruising)
        highway[[i]] <- result$origin
        highway[[i-1]] <- result$target
    }
    list(highway=highway, overflow=overflow)
}

rules.aggressive <- function (highway, overflow) {
    for (i in seq(highway)) {
        lane <- highway[[i]]
        n <- nrow(lane)
        t <- rbindlist(list(lane[-1,], virtual.tofront(lane)))
        accel <- ifelse(lane$crashed, 0, pmin(v.vmax(lane$type) - lane$speed, v.accel(lane$type)))
        safeaccel <- pmin(maxaccel(lane, t), accel)

        target <- if (i < hw$lane) 1 else -1
        slowed <- !lane$crashed & safeaccel < accel & safeaccel <= 0
        result <- changelane(highway, i, target, slowed)
        lane <- result$origin
        highway[[i+target]] <- result$target
        safeaccel <- safeaccel[result$unchanged]

        lane[,speed := pmax(speed + safeaccel, 0)]
        lane[,position := position + speed]
        lane[,lane := lane + 1/duration]
        lane[,last.lanechange := pmax(0, last.lanechange - 1)]
        ahead <- rbindlist(list(lane[-1,], virtual.tofront(lane)))
        crashed <- v.nose(lane) > ahead$position
        lane$crashed <- lane$crashed | crashed | c(tail(crashed, 1), head(crashed, -1))
        lane[, speed := ifelse(crashed, 0, speed)]
        newposition <- lane$position %% hw$length
        overflow[i] <- overflow[i] + sum(lane$position != newposition)
        lane[, position := newposition]
        neworder <- order(lane$position)
        lane <- lane[neworder]
        highway[[i]] <- lane
    }
    list(highway=highway, overflow=overflow)
}

highways <- run(highway, rules.aggressive)

jpeg("test.jpg", height= 540, width=1040)
map <- aes(x=position, y=lane, size=type, color=type)
ggplot() +
    scale_size_manual(values=c(0.1,0.1,0.1),labels=c("Car","Moto","Truck")) +
    geom_point(map, highways) +#, show.legend=F) +
    labs(x="Position (m)",y="Voie/Temps") +
    scale_y_continuous(breaks = 1:hw$lane) +
    # theme_wsj() + scale_colour_wsj("colors6")
    # theme_calc() + scale_color_calc()
    # theme_hc() + scale_color_hc()
    # theme_stat() + scale_color_stat()
    theme_dark() + scale_color_manual(values=c("Black", "Yellow","Blue"))
dev.off()
system("xdg-open test.jpg")
# })

# Base-8000-2sec: 20, 51, 55, 72
# Base-12000-2sec: 4, 44, 57, 80
# Base-16000-2sec: 0, 38, 74, 59
# Aggressive-8000-2sec: 0, 59, 52, 72
# Aggressive-12000-2sec: 0, 39, 52, 86
# Aggressive-16000-2sec: 0, 31, 57, 83
.libPaths('.')
.f <- function () {
library(ggplot2)
models <-
    data.frame(
        model=rep(c(rep("Base",3),rep("Aggr",3)),3),
        length=c(rep("8 km",6),rep("12 km",6),rep("16 km",6)),
        # model=c(rep("Base-8000",3),rep("Aggr-8000",3),rep("Base-12000",3),rep("Aggr-12000",3),rep("Base-16000",3),rep("Aggr-16000",3)),
        # model=c("Base-8000","Aggr-8000","Base-12000","Aggr-12000","Base-16000","Aggr-16000"),
        crash=c(20,0,4,0,0,0),
        vehicules=c(51,55,72,59,52,72,44,57,80,39,52,68,38,74,59,31,57,83),
        lane=rep(c("1","2","3"),6)
    )
jpeg("test.jpg", height= 540, width=1040)
ggplot(data=models,aes(x=model,y=vehicules,fill=lane)) +
    geom_bar(stat="identity") +
    facet_wrap(~length,nrow=1) +
    scale_fill_brewer(palette=16) +
    # theme_stata()
    theme_gray()
#,position=position_dodge())
dev.off()
system("xdg-open test.jpg")
}
