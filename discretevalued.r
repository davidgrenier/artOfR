preda <- function (x, k) {
    n <- length(x)
    pred <- vector(length=n-k)
    for (i in 1:(n-k))
        pred[i] <- if (mean(x[i:(i+k-1)]) >= 0.5) 1 else 0
    mean(abs(pred-x[(k+1):n]))
}

predb <- function (x, k) {
    n <- length(x)
    k2 <- k/2
    pred <- vector(length=n-k)
    sm <- sum(x[1:k])
    pred[1] <- if (sm >= k2) 1 else 0
    if (n-k > 1) {
        for (i in 2:(n-k)) {
            sm <- sm + x[i+k-1] - x[i-1]
            pred[i] <- if (sm >= k2) 1 else 0
        }
    }
    mean(abs(pred-x[(k+1):n]))
}

predc <- function (x, k) {
    n <- length(x)
    k2 <- k/2
    pred <- vector("integer",n-k)
    csx <- c(0, cumsum(x))
    for (i in 1:(n-k))
        pred[i] <- csx[i+k] - csx[i] >= k2
    mean(abs(pred-x[(k+1):n]))
}

predd <- function (x, k) {
    n <- length(x)
    k2 <- k/2
    csx <- c(0, cumsum(x))
    pred <- as.integer(csx[(1+k):n] - head(csx, n-k) >= k2)
    mean(abs(pred-x[(k+1):n]))
}

data <- rep(c(0,1,0,1,0,1,1,1,0,1,0,0,0,1,1,0), 200)
t <- function (f) {
    mapply(function (k) f(data,k), 1:8)
}
#t(preda)
#t(predb)
t(predc)
t(predd)
