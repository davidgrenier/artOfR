i <- 1
while (i <= 10) i <- i+4
i

i <- 1
while(T) {
    if (i > 10) break
    i <- i+4
}
i

i <- 1
repeat {
    i <- i+4
    if (i > 10) break
}
i

sim <- function (nreps) {
    commdata <- list()
    commdata$countabsamecomm <- 0
    for (rep in seq(nreps)) {
        commdata$whosleft <- 1:10
        commdata$numabchosen <- 0

        commdata <- choosecomm(commdata,5)
        if (commdata$numabchosen > 0) next

        commdata <- choosecomm(commdata,4)
        if (commdata$numabchosen > 0) next

        commdata <- choosecomm(commdata,3)
    }
    print(commdata$countabsamecomm/nreps)
}

sim1 <- function (nreps) {
    commdata <- list()
    commdata$countabsamecomm <- 0
    for (rep in seq(nreps)) {
        commdata$whosleft <- 1:10
        commdata$numabchosen <- 0

        commdata <- choosecomm(commdata,5)
        if (commdata$numabchosen == 0) {
            commdata <- choosecomm(commdata,4)
            if (commdata$numabchosen == 0)
                commdata <- choosecomm(commdata,3)
        }
    }
    print(commdata$countabsamecomm/nreps)
}

u <- matrix(c(1:3,1,2,4),3)
v <- matrix(c(8,12,20,15,10,2),3)
for (m in c("u","v")) {
    z <- get(m)
    print(lm(z[,2]~z[,1])$fitted.values)
}

T * T
T * F
T == 1
(0 == 0) == 1

read.table("data/exams",T) #No it is not true you have to name the arguments

# ?"function"
#function

triple <- function (`a name`) do.call("paste",as.list(rep(`a name`,3)))
triple("David")

#?body
test <- function (x) {
    print(body())
    #x(x)
}
# test(test)
body(test)
formals(test)
# ?"{"

# ?.Primitive("(")
# tapply(1:7,c('a','b','a','b','a','b','a'),`(`)

# edit(abline)

g1 <- sin
g2 <- function (x) sqrt(x^2+1)
g3 <- function (x) 2*x+1
pdf("test.pdf")
plot(g3,0,pi)
plot(g1,0,pi,add=T)
plot(g2,0,pi,add=T)
dev.off()
system("xdg-open test.pdf")

g <- function (h,a,b) h(pi)
g
# g(sin)
body(g) <- quote(2*x+3)
g
x <- 3
g()

quote(2*x+3)
# ?quote

w <- 12
f <- function (y) {
    d <- 8
    h <- function () d*(w+y)
    h
}
# environment(f())
# ?.GlobalEnv
ls()
ls.str()
# ?environment
# ?ls

facto <- function (n) if (n < 1) 1 else n * facto(n-1)
facto(5)
