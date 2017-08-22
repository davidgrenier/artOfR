qs <- function (xs) {
    if (length(xs) <= 1)
        return(xs)
    pivot <- xs[1]
    rest <- xs[-1]
    low <- qs(rest[rest < pivot])
    hi <- qs(rest[rest >= pivot])
    c(low,pivot,hi)
}
qs(c(5,4,12,13,3,8,88))

printtree <- function (tr) {
    printtree <- function (hdidx, tr) {
        left <- tr$mat[hdidx,1]
        if (!is.na(left))
            printtree(left,tr)
        print(tr$mat[hdidx,3])
        right <- tr$mat[hdidx,2]
        if (!is.na(right))
            printtree(right,tr)
    }
    printtree(1,tr)
}
newtree <- function (firstval) {
    m <- matrix(,4,3)
    m[1,3] <- firstval
    list(mat=m,nxt=2)
}
insert <- function (tr, value) {
    ins <- function (hdidx) {
        t <- value <= tr$mat[hdidx,3]
        dir <- if (t) 1 else 2
        if (is.na(tr$mat[hdidx,dir])) {
            if (tr$nxt == nrow(tr$mat) + 1)
                tr$mat <- rbind(tr$mat, matrix(,nrow(tr$mat),3))
            tr$mat[tr$nxt,3] <- value
            tr$mat[hdidx,dir] <- tr$nxt
            tr$nxt <- tr$nxt+1
            tr
        } else ins(tr$mat[hdidx,dir])
    }
    ins(1)
}
t <- newtree(2)
t <- insert(t,3)
t <- insert(t,9)
t <- insert(t,4)
t <- insert(t,4)
for (x in seq(1:20))
    t <- insert(t,x)
t
printtree(t)
