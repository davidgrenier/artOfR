z <- read.csv("~/Desktop/2016-2017.csv")
z$Ratio <- z$Positifs/z$Tests
ratio <- z$Ratio
result <- fitted(lm(ratio[-1]~head(ratio,-1)))
y <- ratio[-1]
x <- seq(y)
pdf("test.pdf")
plot(x,y)
lines(predict(loess(y~x)))
dev.off()
system("xdg-open test.pdf")
