d <- c(a=5,b=12,13,4,3,28,12,12,9,5,5,13,5,4,12)
dtab <- table(d)

ct <- read.table("data/ct.dat",T,sep=" ")
cttab <- table(ct)

tabdom <- function (tbl, k) {
    tbldf <- data.frame(tbl)
    freqord <- order(tbldf$Freq,decreasing=T)
    tbldf[freqord,][1:k,]
}

tabdom(dtab,3)
tabdom(cttab,2)

# create.dir("reshape")
# install.packages("reshape",".",repos="http://cran.stat.sfu.ca")
# library(reshape,lib.loc=".")
# ?cast

aba <- read.csv("data/abalone.data")
# by(aba,aba$Gender,function (m) lapply(m[,-1],median))
# tapply(aba$Length,list(aba$Rings,aba$Gender),mean)
aggregate(aba[,-1],list(aba$Gender),median)

z <- c(0.88114802, 0.28532689, 0.58647376, 0.42851862, 0.46881514, 0.24226859, 0.05289197, 0.8803567)
cut(z, seq(0,1,0.1),labels=F)

?findInterval
