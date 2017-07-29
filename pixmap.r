blurpart <- function (img,rows,cols,q) {
    lrows <- length(rows)
    lcols <- length(cols)
    newimg <- img
    randomnoise <- matrix(runif(lrows*lcols), nrow=lrows,ncol=lcols)
    newimg@grey[rows,cols] <- (1 - q) * randomnoise + q * img@grey[rows,cols]
    newimg
}
library(pixmap, lib.loc=".")
mtrush1 <- read.pnm("mtrush1.pgm", cellres=1)
#mtrush1@grey[82:155,133:177] <- 1
mtrush1 <- blurpart(mtrush1, 82:155, 133:177, 0.35)
pdf("test.pdf")
plot(mtrush1)
dev.off()
system("xdg-open test.pdf")
