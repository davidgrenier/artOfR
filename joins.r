d1 <- data.frame(
    kids=c("Jack","Jill","Jillian","John","Jill")
    ,states=c("CA", "MA", "MA", "HI","TX")
)
d2 <- data.frame(
    ages=c(10,7,12)
    ,kids=c("Jill", "Lillian", "Jack")
)
#merge(d1,d2)

d3 <- data.frame(
    ages=c(12,10,7)
    ,pals=c("Jack","Jill","Lillian")
)

#merge(d1,d3,by.x="kids",by.y="pals")

d2a <- rbind(d2,c(15,"Jill"))
d2a
merge(d1,d2a)
