colors <- rainbow(8)
data <- read.csv("AllYears.csv")
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
for (alpha in (10:50)/100) {
    for (beta in (10:min(50,90-alpha))/100) {
        # alpha <- 0.375
        # beta <- 0.75-alpha
        data$metric6 <- alpha*data$Ratio.A + beta*data$Ratio.B + (1-alpha-beta)*metric.Tests
        hideYears <- c(2018,2007)
        subset <- data[data$endIndex < 10 & !data$Saison %in% hideYears,]
        subset$cut <- cut(subset$metric6,breaks=8)
        result <-
            by(subset,subset["cut"],function (entries) {
               list(metric.moy=mean(entries$metric6),
                 metric.ecart=sqrt(var(entries$metric6)),
                 week.moy=round(mean(entries$endIndex),2),
                 week.ecart=round(sqrt(var(entries$endIndex)),2),
                 num=length(unique(entries$Saison)))
            })
        print(paste("alpha: ", alpha, " beta: ", beta))
        # print(result)
        for (entry in result) {
            if (is.null(entry))
                next
            if (entry$num == 6)
                print(unlist(entry))
        }
    }
}

# metric.A = data$Positifs.A/data$max.A
# metric.B = data$Positifs.B/data$max.B
# data$metric <- (alpha*ifelse(is.na(metric.A),1,metric.A)
#     + beta*ifelse(is.na(metric.B),1,metric.B)
#     + (1-alpha-beta)*ifelse(is.na(metric.Tests),1,metric.Tests));
# data$metric2 <- data$Ratio + 0.4*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric3 <- data$Ratio + 0.3*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric4 <- data$Ratio + 0.45*ifelse(is.na(metric.Tests),1,metric.Tests)
# data$metric5 <- data$Ratio + 0.55*ifelse(is.na(metric.Tests),1,metric.Tests)

data$endIndex <- max(data$endIndex)-data$endIndex

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
system("xdg-open test.pdf")
# system("explorer test.pdf")
# ?pdf
