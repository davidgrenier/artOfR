tapply(c(25,26,55,37,21,42),c('R','D','D','R','U','D'),mean)

v1 <- c('R','D','D','R','U','D')
v2 <- c(1,2,1,2,2,1)
tapply(c(25,26,55,37,21,42),list(v1,v2),c)

u <- c(22,8,33,6,8,29,-2)
f <- list(c(5,12,13,12,13,5,13),c('a','bc','a','a','bc','a','a'))
tapply(u,f,length)

z <- list(gender=c('M','M','F','M','F','M'),age=c(47,59,21,32,33,24),income=c(55000,88000,32450,76500,123000,45650))
tapply(z$income,list(z$gender,z$age>25),mean)

tapply(1:7,c('m','f','f','i','m','m','f'),identity)

gender <- c('M','M','F','M','F','F')
age <- c(47,59,21,32,33,24)
income <- c(55000,88000,32450,76500,123000,45650)
z <- data.frame(gender,age,income)
split(z$income,list(z$gender,z$age > 25))
tapply(z$income,list(z$gender,z$age>25),c)

split(1:7,list(c(5,12,13,12,13,5,13),c('a','bc','a','a','bc','a','a')))
u <- c(22,8,33,6,8,29,-2)
f <- list(c(5,12,13,12,13,5,13),c('a','bc','a','a','bc','a','a'))
tapply(u,f,length)
