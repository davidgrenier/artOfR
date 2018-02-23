data <- read.csv("AllYears.csv")
data <- data[!data$Saison %in% c(2007),]
colors <- rainbow(length(unique(data$Saison)))
names(colors) <- unique(data$Saison)
levels <- unique(c(data[data$Saison==2015,]$Semaine,data$Semaine))
withLabels <- seq(levels)
names(withLabels) <- levels
semaines <- sapply(data$Semaine,function (sem) unname(withLabels[as.character(sem)]))
data$Semaine <- ordered(semaines,seq(levels),labels=levels)
data$Positifs <- data$Positifs.A+data$Positifs.B
data$Ratio.A <- data$Positifs.A/data$Tests
data$Ratio.B <- data$Positifs.B/data$Tests
data$Ratio <- data$Ratio.A+data$Ratio.B
data$endIndex <-
    unlist(by(data$Semaine,data[1],function (entries) {
        rev(seq(entries))
    }))
# data$max.A <- unlist(by(data$Positifs.A,data[1],cummax))
# data$max.B <- unlist(by(data$Positifs.B,data[1],cummax))
data$max.Tests <- unlist(by(data$Tests,data[1],cummax))
metric.Tests = data$Tests/data$max.Tests
# for (alpha in (10:50)/100) {
#     for (beta in (10:min(50,90-alpha))/100) {
for (alpha in 0.77) {
    for (cuts in 9) {
        beta <- 1-alpha
        data$metric6 <- alpha*data$Ratio + beta*metric.Tests
        hideYears <- c(2018,2007)
        subset <- data[data$endIndex < 10 & !data$Saison %in% hideYears,]
        subset$cut <- cut(subset$metric6,breaks=cuts)
        result <-
            by(subset,subset["cut"],function (entries) {
               list(metric.moy=mean(entries$metric6),
                 # metric.ecart=sqrt(var(entries$metric6)),
                 week.moy=round(mean(entries$endIndex),2),
                 week.ecart=round(sqrt(var(entries$endIndex)),2),
                 week.ecartP=round(sqrt(var(entries$endIndex)),2)/round(mean(entries$endIndex),2),
                 num=length(unique(entries$Saison)))
            })
        # print(result)
        for (entry in result) {
            if (is.null(entry))
                next
            if (entry$num == 6 & entry$week.moy > 7 & entry$metric.moy > 0.14  & entry$metric.moy < 0.15 & entry$week.ecartP<0.12) {
                print(paste("alpha: ", alpha, " cuts: ", cuts))
                print(unlist(entry))
            }
        }
    }
}
# [1] "alpha:  0.77  cuts:  9"
#  metric.moy    week.moy  week.ecart week.ecartP         num 
#   0.1479733   7.7500000   0.7100000   0.0916129   6.0000000 

y = function (x) x*7.75/0.1479

data$prediction <- y(data$metric6)

# metric.A = data$Positifs.A/data$max.A
# metric.B = data$Positifs.B/data$max.B
# data$metric <- (alpha*ifelse(is.na(metric.A),1,metric.A)
#     + beta*ifelse(is.na(metric.B),1,metric.B)
#     + (1-alpha-beta)*ifelse(is.na(metric.Tests),1,metric.Tests));
# data$metric2 <- data$Ratio + 0.4*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric3 <- data$Ratio + 0.3*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric4 <- data$Ratio + 0.45*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric5 <- data$Ratio + 0.55*ifelse(is.na(metric.Tests),1,metric.Tests)

# data$endIndex <- max(data$endIndex)-data$endIndex
data[c("Saison","Semaine","endIndex","prediction")]
pd <- data[c("Saison","Semaine","endIndex","prediction")][data$Semaine > 10 & data$endIndex > 1,]
pd$diff <- abs(pd$prediction-pd$endIndex)/pd$prediction*100
pd
mean(pd$diff)
sqrt(var(pd$diff))

# pdf("test.pdf",width=12,height=7.5)
png("test.png",width=640,height=640)
weeks <- ordered(seq(levels),labels=levels)
field <- "prediction"
# data <- data[!data$Saison %in% c(2007,2018),]
plot(weeks,c(0,rep(NA,length(weeks)-2),max(data[field])),main="",xlab="semaine r\u00e9elle (i)",ylab="pr\u00e9diction (S(i))")
# plot(1:35,c(0,rep(NA,33),max(data[field])),main="",xlab="i",ylab=expression(phi))
legend("topleft",legend=names(colors),fill=colors)
for (year in unique(data$Saison)) {
    saison <- data[data$Saison==year,]
    # lines(saison$endIndex,saison[[field]],col=colors[as.character(year)])
    lines(saison$Semaine,saison[[field]],col=colors[as.character(year)])
}
dev.off()
# system("xdg-open test.pdf")
system("explorer test.png")
