x <- c(88, NA, 12, 168, 13)
mean(x)
mean(x, na.rm=T)
x <- c(88, NULL, 12, 168, 13)
mean(x)

x <- c(5,NA,12)
mode(x)
mode(x[3])
x <- c("abc", "def", NA)
mode(x)
mode(x[3])

z <- NA
for (i in 1:10) if (i %% 2 == 0) z <- c(z,i)
z
seq(2,10,2)
2*1:5

length(NULL)
length(NA)

z <- 1:10
z[z*z >= 9]

a <- z*z
b <- ">="(a, 9)
r <- z[b]
r

z <- c(5,2,-3,8)
j <- z*z > 8
y <- c(1,2,30,5)
j
y
y[j]

as.integer(c(1,2,3))

x <- 1:10
x[x > 3] <- 0
x

x <- c(6,1:3,NA,12)
x > 4
x[x > 4]
x[c(T,F,T)]
subset(x, x == x)
z <- c(5,2,-3,8)
which(z*z > 8)

first1 <- function (x) {
    for (i in 1:length(x))
        if (x[i] == 1) break
    i
}
first1(c(2,1,0))
first1a <- function (x)
    which(x==1)[1]
first1a(c(2,1,0))

x <- 1:10
y <- ifelse(x %% 2 == 0,5,12)
y

x <- c(5,2,9,12)
2*x
3*x
ifelse(x > 6, 2*x, 3*x)
