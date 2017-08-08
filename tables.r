u <- c(22,8,33,6,8,29,-2)
fl <- list(c(5,12,13,12,13,5,13),c('a','bc','a','a','bc','a','a'))
tapply(u,fl,length)
table(fl)
# table(fl[[1]])
# table(fl[[2]])

v <- data.frame(gender=c('M','M','F','M','F','F')
                ,tone=c('W','W','A','O','B','B')
                ,pol=c('L','L','C','L','L','C'))
table(v)
# apply(table(v),3,sum)

ct <- read.table("data/ct.dat",header=T)
cctab <- table(ct)
# cctab
# dimnames(cctab)
# class(cctab)
# cctab[1,1]
# cctab[1,]
# cctab[,1]
# cctab/sum(cctab)
# apply(cctab,1,sum)
# apply(cctab,2,sum)
# addmargins(cctab)
subtable <- function (tbl,subnames) {
    tblarray <- unclass(tbl)
    dcargs <- list(tblarray)
    for (i in seq(subnames))
        dcargs[[i+1]] <- subnames[[i]]
    subarray <- do.call("[",dcargs)
    dims <- lapply(subnames,length)
    subtbl <- array(subarray,dims,subnames)
    class(subtbl) <- "table"
    subtbl
}
cctab[c("No","Yes"),c("No","Yes")]
subtable(cctab,list(q=c("No","Yes"),r=c("No","Yes")))
