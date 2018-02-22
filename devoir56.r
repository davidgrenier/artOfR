colors <- rainbow(8)
data <- read.csv("AllYears.csv")
data <- data[data$Saison != 2007,]
data <- data[data$Saison != 2018,]
names(colors) <- unique(data$Saison)
levels <- unique(c(data[data$Saison==2015,]$Semaine,data$Semaine))
withLabels <- seq(levels)
names(withLabels) <- levels
semaines <- sapply(data$Semaine,function (sem) unname(withLabels[as.character(sem)]))
data$Semaine <- ordered(semaines,seq(levels),labels=levels)
data$Positifs <- data$Positifs.A+data$Positifs.B
# data$Ratio.A <- data$Positifs.A/data$Tests
# data$Ratio.B <- data$Positifs.B/data$Tests
data$Ratio <- (data$Positifs.A+data$Positifs.B)/data$Tests
data$endIndex <-
    unlist(by(data$Semaine,data[1],function (entries) {
        rev(seq(entries))
    }))
# data$max.A <- unlist(by(data$Positifs.A,data[1],cummax))
# data$max.B <- unlist(by(data$Positifs.B,data[1],cummax))
data$max.Tests <- unlist(by(data$Tests,data[1],cummax))
# alpha <- 0.4
# beta <- 0.4
# metric.A = data$Positifs.A/data$max.A
# metric.B = data$Positifs.B/data$max.B
metric.Tests = data$Tests/data$max.Tests
# data$metric <- (alpha*ifelse(is.na(metric.A),1,metric.A)
#     + beta*ifelse(is.na(metric.B),1,metric.B)
#     + (1-alpha-beta)*ifelse(is.na(metric.Tests),1,metric.Tests));
# data$metric2 <- data$Ratio + 0.4*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric3 <- data$Ratio + 0.3*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric4 <- data$Ratio + 0.45*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric5 <- data$Ratio + 0.55*ifelse(is.na(metric.Tests),1,metric.Tests)
data$metric6 <- 0.75*data$Ratio + 0.25*ifelse(is.na(metric.Tests),1,metric.Tests)
subset <- data[data$endIndex < 10,]
subset$m6 <- cut(subset$metric6,breaks=8)
by(subset,subset["m6"],function (entries) {
   c(mm=mean(entries$metric6),etm=sqrt(var(entries$metric6)),ms=mean(entries$endIndex),ets=sqrt(var(entries$endIndex)),num=length(unique(entries$Saison)))
})

# pdf("test.pdf",width=480,height=480)
pdf("test.pdf",width=12,height=7.5)
# weeks <- ordered(seq(levels),labels=levels)
# plot(weeks,c(0,rep(NA,length(weeks)-2),1))
field <- "metric6"
plot(1:35,c(0,rep(NA,33),max(data[field])),main=field)
legend("topleft",legend=names(colors),fill=colors)
for (year in unique(data$Saison)) {
    saison <- data[data$Saison==year,]
    lines(saison$endIndex,saison[[field]],col=colors[as.character(year)])
    # lines(saison$Semaine,saison$metric,col=colors[as.character(year)])
}
dev.off()
# system("xdg-open test.png")
system("explorer test.pdf")
# ?pdf
