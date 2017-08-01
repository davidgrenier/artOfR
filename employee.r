file1 <- "data/DA"
file2 <- "data/DB"
# count.fields(file1,sep=",")
# all(count.fields(file1,sep=",") == 5)
# table(count.fields(file1,sep=","))
da <- read.csv(file1,stringsAsFactors=F)
da
db <- read.csv(file2,header=F,stringsAsFactors=F)
db
merge(da,db,by.x=1,by.y=1)
