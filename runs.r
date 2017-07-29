data <- rep(c(1,1,0,0,1,1,1,0,1,1,0,1,1,1), 100000)

findruns <- function (runs, k) {
    for (i in seq(k-1))
        runs <- runs & c(runs[-1], 0)
    which(runs)
}

system.time(findruns(data, 3))
mean(c(T,F,T,F,T,T,F,T,F))

q <- cumsum(1:9); q[3:9] - c(0, q[1:6])
?seq
seq(1, 2, length.out=10)
round(seq(1.1, 2, length.out=10))

cbind(1:3) %*% 4:1
f <- function (z) c(z, z^2)
sapply(1:10, f)
sapply(1:8, function (x) c(x, x^2))
matrix((function (x) c(x, x^2))(1:8), ncol=2)
mean(c(88, NA, 12, 168, 13), na.rm=T)

seq(c(1, NA, 3)) - seq(c(1, NULL, 2, 3))
z <- c(5,2,-3,8)
">"("^"(z,2), 8)
z <- 1:10; z[z %% 2 == 0] <- 0; z
1:10 * (1:10 > 3)

z <- c(6,1,2,3,NA,12)
z > 5
z[z > 5]
z[which(z > 5)]

subset(z, z > 5)
ifelse(1:10 %% 2 == 0, 5, 12)

lapply(list(1, 2), sqrt)
m <- "M"; f <- "F"; g <- c(m, f, f, "I", m, m, f); ifelse(g == m, 1, ifelse(g == f, 2, 3))
