findruns1 <- function (x, k) {
    n <- length(x)
    runs <- vector("integer", n)
    #runs <- NULL
    count <- 0
    for (i in 1:(n-k+1)) {
        if (all(x[i:(i+k-1)] == 1)) {
            count <- count + 1
            runs[count] <- i
            #runs <- c(runs, i)
        }
    }
    #runs
    runs[seq_len(count)]
}
findruns1(rep(c(1,1,0,1,1,1,0,1,1), 100000), 3)
