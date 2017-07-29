mode(c(T,F))
mode(1:2)
mode(c("a"))
mode(c(2.3))
q <- c(T,F)
mode(q)
typeof(c(1,2,3))
typeof(1:3)

10:13 == rbind(c(10,12),c(11,13))
args(vector)
vector("character", 2)
cbind(c(2,4,4),c(6,6,8)) == cbind(1:3,4:6) + c(1,2)

?matrix
as.matrix(1:10, 3)

(1:10)[-2:-9]
x <- c(5,12,13); x[-length(x)]
x[c(1,1)]

?Syntax
rep(1:3,3)
which(seq(10) > 8)
which(seq(10) > 4)
