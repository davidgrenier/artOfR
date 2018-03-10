# install.packages("zeallot",'.',dep=T,repos="http://cran.stat.sfu.ca")
.libPaths('.')
library(ggplot2)
library(zeallot)
library(data.table)
geom <- function (xs) exp(sum(log(xs[xs>0]),na.rm=T)/length(xs))
set.seed(334)
safedistance <- 10# 2sec*speed ?!?
levels <- factor(1:3)
c(car,moto,truck) %<-% levels
v.length <- function (vs) ifelse(vs == moto, 2.2, ifelse(vs == car, 4.5, 14.6))
v.mass <- function (vs) ifelse(vs == moto, 175, ifelse(vs == car, 1850, 9750))
v.accel <- function (vs) ifelse(vs == moto, 7, ifelse(vs == car, 3, 0.6))
# v.vmax <- function (vs) ifelse(vs == truck, 29, 32.5)
v.vmax <- function (vs) ifelse(vs == truck, 29, ifelse(vs == moto, 50, 32.5)) #faster moto
v.random <- function (n) {
    vehicles <- rbinom(n,1,0.73)
    # factor(ifelse(vehicles, vehicles, rbinom(n,1,0.25/0.27)+2),levels)
    factor(ifelse(vehicles, vehicles, rbinom(n,1,0.15/0.27)+2),levels) #more moto
}
hw <- list(lanes = 3, vehicles = 200, length = 16000)
lane.random <- function (lane) {
    n <- hw$vehicles/ifelse(lane==1||lane==hw$lanes,2,1)
    position <- (1:n)*(hw$length/n)
    # position <- sort(unique(trunc(runif(2*n, 1, hw$length)))[1:n])
    type <- v.random(n)
    speed <- (rbeta(n,2,0.5)/2+0.25)*v.vmax(type)
    data.frame(position, type, lane, original = lane, speed, crashed=F)
}
highway <- lapply(seq(hw$lane), lane.random)

stepby <- 2
duration <- stepby*100
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
        # print(c("time", time))
        highway <- rules(highway)
        time <- time+1
    }
    data.frame(rbindlist(allcars))
}

v.nose <- function (lane) lane$position + v.length(lane$type)
v.safe <- function (lane) v.nose(lane) + safedistance
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

rules.basic <- function (highway) {
    cartoofar <- list(position=9999999,type=car,lane=0,original=0,speed=0,crashed=F)
    cartoobehind <- list(position=-9999999,type=car,lane=0,original=0,speed=0,crashed=F)
    for (i in seq(highway)) {
        lane <- highway[[i]]
        n <- nrow(lane)
        t <- rbind(tail(lane,-1),cartoofar)
        accel <- ifelse(lane$crashed, 0, pmin(v.vmax(lane$type) - lane$speed, v.accel(lane$type)))
        safeaccel <- pmin(maxaccel(lane, t), accel)
        slowed <- !lane$crashed & safeaccel < accel
        if (i < hw$lanes && any(slowed)) {
            left <- highway[[i+1]]
            checkbroken("broken-before", left)
            tokeep <- rep(T,nrow(lane))
            for (j in which(slowed)) {
                v <- lane[j,]
                insertat <- which(left$position > v.safe(v))[1]
                if (is.na(insertat) || maxaccel(v, left[insertat,]) > max(safeaccel, 0)) {
                    insertat <- if (is.na(insertat)) nrow(left)+1 else insertat
                    if (insertat > 1 && v.safe(left[insertat-1,]) > v$position)
                        next
                    before <- row(left)[,1] < insertat
                    v$lane <- v$lane + 1
                    left <- rbindlist(list(left[before,], v, left[!before,]))
                    tokeep[j] <- F
                    checkbroken("broken-after", left, v)
                }
            }
            lane <- lane[tokeep,]
            safeaccel <- safeaccel[tokeep]
            rownames(lane) <- NULL
            highway[[i+1]] <- left
        }
        lane$speed <- pmax(lane$speed + safeaccel,0)
        lane$position <- lane$position + lane$speed
        lane$lane <- lane$lane + 1/duration
        crashed <- v.nose(head(lane, -1)) > tail(lane, -1)$position
        lane$crashed <- lane$crashed | c(crashed,F) | c(F,crashed)
        lane$speed <- ifelse(lane$crashed, 0, lane$speed)
        highway[[i]] <- lane
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
