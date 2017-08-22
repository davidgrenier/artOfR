newbookvec <- function (x) {
    tmp <- list(vec=x,writes=rep(0,length(x)))
    class(tmp) <- "bookvec"
    tmp
}
"[.bookvec" <- function (bv, subs) bv$vec[subs]
"[<-.bookvec" <- function (bv, subs, value) {
    bv$writes[subs] <- bv$writes[subs] + 1
    bv$vec[subs] <- value
    bv
}
b <- newbookvec(c(3,4,5,5,12,13))
b
b[2]
b[2] <- 88
b[2]
b
