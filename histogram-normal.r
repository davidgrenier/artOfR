pdf("test.pdf")
hist(rnorm(100))
dev.off()
system("xdg-open test.pdf")
