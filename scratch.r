aba <- read.csv("data/abalone.data")
by(aba,aba[1],function (m) lm(m[,2]~m[,3]))
