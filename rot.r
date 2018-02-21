library(pixmap,lib.loc='.')
mtrush <- read.pnm("mtrush1.pgm")

rows <- row(mtrush@grey)
cols <- col(mtrush@grey)
rot <- matrix(c(0,-1,1,0),2)

indexes <- mapply(function (a,b) rot %*% c(a,b),rows,cols,SIMPLIFY=F)
indexes

do.call("[", c(list(mtrush@grey),indexes))

png("~/Desktop/test.png")
plot(mtrush)
dev.off()
system("xdg-open ~/Desktop/test.png")
