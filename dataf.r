kids <- c("Jack", "Hill")
ages <- c(12,10)
lastNames <- c("Tom", "Zerry")
d <- data.frame(kids,ages,lastNames,stringsAsFactors=F)
#rbind(d,list("Laura", 19, "Bond"))
d
sapply(d,max)

str(d)
d[,1]
d[[1]]
d[["kids"]]
d$kids
d[c("kids","ages")]

examsquiz <- read.table("data/exams",header=T)
#examsquiz[2:5,]
#examsquiz[2:5,2]
#examsquiz[2:5,2,drop=F]
#class(examsquiz[2:5,2,drop=F])
#examsquiz[examsquiz$Exam.1 >= 3.8,]
#apply(examsquiz,1,mean,na.rm=T)
#subset(examsquiz,examsquiz$Exam.1 >= 3.8)
#subset(examsquiz,examsquiz$Exam.1 >= 3.8)
#complete.cases(examsquiz)
#examsquiz[complete.cases(examsquiz),]
#cbind(examsquiz,T=1:nrow(examsquiz))
#cbind(examsquiz,diff=examsquiz$Exam.2-examsquiz$Exam.1)
#examsquiz$diff <- examsquiz$Exam.2-examsquiz$Exam.1
#rbind(examsquiz,c(Exam.2=3,Quiz=4,Exam.1=4))
#examsquiz$one <- 1
#apply(examsquiz,2,max,na.rm=T)
#lapply(examsquiz,max,na.rm=T)
class(examsquiz)
sapply(examsquiz,function (x) rep(max(x,na.rm=T),2))
