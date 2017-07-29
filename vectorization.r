z12 <- function (z) c(z, z^2)
sapply(1:8, z12)
matrix(z12(1:8), 8)

x <- 1:10
y <- ifelse(x %% 2, 5, 12)
y

x <- c(5,2,9,12)
ifelse(x > 6, 2*x, 3*x)

findud2 <- function (v) {
    vud <- v[-1] - v[-length(v)]
    ifelse(vud > 0, 1, -1)
}

findud <- function (v)
    sign(diff(v))
udcorr <- function (x,y)
    mean(findud(x) == findud(y))

x = c(5,12,13,3,6,0,1,15,16,8,88)
y = c(4,2,23,23,6,10,11,12,6,3,2)
udcorr(x, y)

u <- c(1,6,7,2,3,5)
diff(u)
