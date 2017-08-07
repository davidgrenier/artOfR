x <- c(5,12,13,12)
xf <- factor(x)
# xf
# str(xf)
# unclass(xf)
# ?unclass
# attr(xf,"levels")
# length(xf)
# xff <- factor(x,levels=c(5,12,13,88))
xff <- factor(x,c(5,12,13,88))
# xff
# xff[2] <- 88
# xff
xff[2] <- 28

ages <- c(25,26,55,37,21,42)
affils <- c("R","D","D","R","U","D")
tapply(ages,affils,mean)

d <- data.frame(gender=c("M","M","F","M","F","F")
                ,age=c(47,59,21,32,33,24)
                ,income=c(55000,88000,32450,67500,123000,45650))
d$over25 <- d$age > 25
# split(d$income,d[c(1,4)])
# tapply(d$income,d[c(1,4)],mean)

# split(d[-3],d[c(1,4)])
#invalid tapply(d[-3],d[c(1,4)],function (x,y) print(x))

aba <- read.csv("data/abalone.data")
# g <- aba$Gender; split(seq(g),g)
byGender <- by(aba,aba$Gender,function (m) lm(m[,2] ~ m[,3]))
fittedF <- byGender$F$fitted.values
# pdf("test.pdf")
# plot(aba[aba$Gender == "F",]$Length,fittedF,pch=c('x','.'))
# dev.off()
# system("xdg-open test.pdf")

txt <- scan("data/testconcord.txt","")
words <- split(seq(txt),txt)
words[order(sapply(words,length))]
