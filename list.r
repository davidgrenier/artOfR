file <- "data/testconcord.txt"

findwords <- function (tf) {
    txt <- scan(tf, "")
    wl <- list()
    for (i in seq(txt)) {
        wrd <- txt[i]
        wl[[wrd]] <- c(wl[[wrd]],i)
    }
    wl
}

r <- findwords(file)

alphawl <- function (wrdlst)
    wrdlst[sort(names(wrdlst))]

#alphawl(r)

freqwl <- function (wrdlst)
    wrdlst[order(sapply(wrdlst,length))]

pdf("test.pdf")
freqs <- freqwl(r)
n <- length(freqs)
barplot(sapply(freqs[round(0.8*n):n],length))
dev.off()
system("xdg-open test.pdf")

data <- read.csv("data/abalone.data")
genders <- sapply(c('M','F','I'), function (gender) which(data$Gender == gender))
data[genders$M,]

b <- list(u=5,v=12)
c <- list(w=13)
a <- list(b,c)
a

z <- c(list(a=1,b=2,c=list(d=5,e=9)))
q <- c(list(a=1,b=2,c=list(d=5,e=9)),recursive=T)
unlist(z) == q
