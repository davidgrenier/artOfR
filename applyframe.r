d <- data.frame(kids=c("Jill", "Jack"), ages=c(10, 12), stringsAsFactors=F)
# d[order(d$ages),]
dl <- lapply(d,sort)
dl
as.data.frame(dl)


aba <- read.csv("data/abalone.data",header=T)
abamf <- aba[aba$Gender != 'I',]
# glm(abamf[1,1,drop=F] ~ abamf[1,-1], family=binomial)
lftn <- function (clmn) {
    glm(abamf$Gender ~ clmn, family=binomial)$coef
}
loall <- sapply(abamf[,-1],lftn)
