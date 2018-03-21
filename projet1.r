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
v.vmax <- function (vs) ifelse(vs == car, 30, ifelse(vs == truck, 27, 33))
x.second.rule <- 2
hw <- list(lanes = 3, vehicles = 200, length = 12000, change.period = 20)
v.random <- function (n, lane) {
    against <- if (lane == hw$lane) 0.75 else 1
    carratio <- 0.73/against
    truckratio <- if (lane == hw$lane) 0 else 0.92/against
    vehicles <- rbinom(n, 1, carratio)
    factor(ifelse(vehicles, vehicles, rbinom(n, 1, truckratio) + 2), levels)
}
lane.random <- function (lane) {
    n <- hw$vehicles/ifelse(lane==1||lane==hw$lanes,2,1)
    position <- (1:n)*(hw$length/n)
    type <- v.random(n, lane)
    speed <- (rbeta(n,2,0.5)/2+0.25)*v.vmax(type)
    data.table(position, type, lane, original = lane, speed, crashed=F, last.lanechange=0)
}
highway <- lapply(seq(hw$lane), lane.random)

stepby <- 2
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

changelane <- function (highway, o, t, candidates) {
    origin <- highway[[o]]
    target <- highway[[o+t]]
    tokeep <- rep(T, nrow(origin))
    candidates <- candidates & origin$last.lanechange == 0 & (o+t < hw$lanes | origin$type != truck)
    if (any(candidates)) {
        checkbroken("broken-before", target)
        for (j in which(candidates)) {
            v <- origin[j,]
            safe.ahead <- c(target$position, virtual.tofront(target)$position) > v.safe(v)
            behind <- rbind(virtual.toback(target), target)
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

rules.basic <- function (highway) {
    # virtual.ahead <- list(position=.Machine$integer.max,type=car,lane=0,original=0,speed=0,crashed=F,last.lanechange=0)
    for (i in seq(highway)) {
        lane <- highway[[i]]
        n <- nrow(lane)
        t <- rbind(lane[-1,], virtual.tofront(lane))
        accel <- ifelse(lane$crashed, 0, pmin(v.vmax(lane$type) - lane$speed, v.accel(lane$type)))
        safeaccel <- pmin(maxaccel(lane, t), accel)
        if (i < hw$lane) {
            slowed <- !lane$crashed & safeaccel < accel & safeaccel <= 0
            result <- changelane(highway, i, 1, slowed)
            lane <- result$origin
            highway[[i+1]] <- result$target
            safeaccel <- safeaccel[result$unchanged]
        }
        lane$speed <- pmax(lane$speed + safeaccel, 0)
        newpos <- (lane$position + lane$speed) %% hw$length
        # if (any(newpos < lane$position)) {
        #     q <- lane
        #     q$newpos <- newpos
        #     print(rbindlist(list(q[1:10,],tail(q,10))))
        # }
        lane$position <- newpos
        lane$lane <- lane$lane + 1/duration
        lane$last.lanechange <- pmax(0, lane$last.lanechange-1)
        print(rbindlist(list(lane[1:3,],tail(lane,3))))
        ahead <- rbind(lane[-1,], virtual.tofront(lane))
        crashed <- v.nose(lane) > ahead$position
        lane$crashed <- lane$crashed | crashed | c(tail(crashed, 1), head(crashed, -1))
        lane$speed <- ifelse(lane$crashed, 0, lane$speed)
        neworder <- order(lane$position)
        lane <- lane[neworder]
        print(rbindlist(list(lane[1:3,],tail(lane,3))))
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

# pdf("test.pdf",width=12)
jpeg("test.jpg", height= 540, width=1040)
map <- aes(x=position, y=lane, size=type, color=type)
ggplot() +
    scale_size_manual(values=c(0.1,0.1,0.1),labels=c("Car","Moto","Truck")) +
    scale_color_manual(values=c("Black", "Yellow","Blue")) +
    geom_point(map, highways, show.legend=F) +
    theme_dark()
dev.off()
system("xdg-open test.jpg")
warnings()
# system("xdg-open test.pdf")
# system("explorer test.pdf")
