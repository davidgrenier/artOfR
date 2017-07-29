examsquiz <- read.table("ExamsQuiz.txt")
lma <- lm(examsquiz[,2] ~ examsquiz[,1])
lmb <- lm(examsquiz[,2] ~ examsquiz[,1] + examsquiz[,3])
summary(lmb)
