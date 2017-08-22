eventrow <- function (eventtime, eventtype, appin=NULL)
    data.frame(c(data.frame(eventtime, eventtype), appin))

scheduleevent <- function (eventtime, eventtype, appin) {
    newevent <- eventrow(eventtime, eventtype, appin)
    if (is.null(sim$evnts)) {
        sim$events <<- newevent
        return()
    }

    events <- sim$events
    insertat <- binsearch(events$eventtime, eventtime)
    before <- if (insertat == 1) NULL else events[1:(insertat-1),]

    numrows <- nrow(events)
    after <- if (insertat > numrows) NULL else events[insertat:numrows,]

    sim$events <<- rbind(before, newevent, after)
}

binsearch <- function(xs, x) {
    lo <- 1
    hi <- length(xs)
    while (lo+1 < hi) {
        mid <- floor((lo+hi) / 2)
        if (x == xs[mid])
            return(mid)

        if (x < xs[mid]) hi <- mid else lo <- mid
    }

    if (x <= xs[lo]) lo
    else if (x < xs[hi]) hi
    else hi+1
}

getnextevent <- function () {
    head <- sim$events[1,]
    sim$events <<- if (nrow(sim$events) == 1) NULL else sim$events[-1,]
    head
}

dosim <- function (initglobals, reactevent, printresults, maxsimtime, apppars=NULL,debug=F) {
    sim <<- list(currenttime=0.0, events=NULL, debug=debug)
    initglobals(apppars)

    while (sim$currenttime < maxsimtime) {
        head <- getnextevent()
        sim$currenttime <<- head$eventtime
        reactevent(head)
        if (debug) print(sim)
    }

    printresults()
}

mm1initglobals <- function (apppars) {
    mm1globals <<- list(
        arrivalrate=apppars$arrivalrate
        , servicerate=apppars$servicerate
        , servicequeue=vector()
        , jobsdone=0
        , totalwaittime=0.0)

    arrivaltime <- rexp(1,mm1globals$arrivalrate)

    scheduleevent(arrivaltime,"arrv",c(data.frame(arrivaltime)))
}

mm1reactevent <- function (head) {
    if (head$eventtype == "arrv") {
        if (length(mm1globals$servicequeue) == 0) {
            donetime <- sim$currenttime + rexp(1,mm1globals$servicerate)
            scheduleevent(donetime,"srvdone",list(arrivaltime=head$arrivaltime))
        } else mm1globals$servicequeue <<- c(mm1globals$servicequeue,head$arrivaltime)

        arrivaltime <- sim$currenttime + rexp(1,mm1globals$arrivalrate)
        scheduleevent(arrivaltime,"arrv",c(data.frame(arrivaltime)))
    } else {
        mm1globals$jobsdone <<- mm1globals$jobsdone+1
        mm1globals$totalwaittime <<- mm1globals$totalwaittime + sim$currenttime - head$arrivaltime
        mm1globals$servicequeue <<- mm1globals$servicequeue[-1]

        if (length(mm1globals$servicequeue) > 0) {
            servicedonetime <- sim$currenttime + rexp(1,mm1globals$servicerate)
            scheduleevent(servicedonetime,"srvdone",list(arrivaltime=mm1globals$servicequeue[1]))
        }
    }
}

mm1printresults <- function () {
    print("mean wait:")
    print(mm1globals$totalwaittime/mm1globals$jobsdone)
}

dosim(mm1initglobals, mm1reactevent, mm1printresults, 1e4, list(arrivalrate=0.5,servicerate=1.0))
