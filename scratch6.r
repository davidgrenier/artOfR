u <- c(22,8,33,6,8,29,-2)

f <- list(c(5,12,13,12,13,5,13),c('a','bc','a','a','bc','a','a'))
table(f)
?table


y <- "Yes"
n <- "No"
z <- data.frame(be4=c(y,y,n,"Not Sure",n),after=c(y,n,n,y,n))
table(z)
data.frame(table(z))

matrix(c(2,0,1,0,1,1),3,dimnames=list(Voted=c("No", "Not Sure", "Yes"),Last=c("No","Yes")))

m <- 'm'; f <- 'f'; l <- 'l'
vt <- data.frame(gender=c(m,m,f,m,f,f),race=c('w','w','a','o','b','b'),pol=c(l,l,'c',l,l,'c'))
t <- table(vt)

q <- list(age=1:3,name=1:3)
q[5:7] <- 1:3
q

t <- table(list(age=1:4,name=c('a','b','c','a')))
t

subtable <- function (tbl, subnames) {
    dcargs <- c(list(tbl),subnames)
    subarray <- do.call("[",dcargs)
    dims <- lapply(subnames,length)
    rez <- array(subarray,dims,subnames)
    class(rez) <- "table"
    rez
}
subtable(array(c(1,2,3),dim=c(1,1,1)),list(bob=1,name=1,q=1))

as.list(rep(1,3))

c(list(table(c(1,2,1,3))),list(1,2,3))

n <- 5
z <- array(round(rnorm(n^n)*10),dim=rep(n,n))
z
dim(z)/2
do.call("[",c(list(z),ceiling(dim(z)/2)))

v <- data.frame(a=c('c','l','c','l','l','c','l','l'),b=c('c','l','l','c','c','c','l','l'))
z <- data.frame(table(v))
z[order(z$Freq,decreasing=T),][1:3,]
