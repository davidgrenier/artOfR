all2006 <- read.csv("data/2006.csv.short",header=T,as.is=T)
all2006 <- all2006[all2006$Wage_Per=="Year",]
#all2006 <- all2006[all2006$Wage_Offered_From > 20000,]
all2006 <- all2006[all2006$Prevailing_Wage_Amount > 200,]
all2006$rat <- all2006$Wage_Offered_From / all2006$Prevailing_Wage_Amount
medrat <- function (frame) median(frame$rat,na.rm=T)
se2006 <- all2006[grep("Software Engineer",all2006),]
makecorp <- function (corpname) all2006[all2006$Employer_Name == corpname,]

corplist <- c("MICROSOFT CORPORATION","ms","INTEL CORPORATION","intel"
              ,"SUN MICROSYSTEMS, INC.", "sun", "GOOGLE INC.", "google")
for (i in 1:(length(corplist)/2)) {
    corp <- corplist[2*i-1]
    newdtf <- paste(corplist[2*i],"2006",sep="")
    assign(newdtf,makecorp(corp),pos=.GlobalEnv)
}
