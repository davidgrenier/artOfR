xs <- rnorm(100)
mySd <- function (xs) {
    avg = mean(xs)
    dev = (xs - avg)^2
    sqrt(mean(dev))
}
mean(xs)
mySd(xs)
sd(xs)
