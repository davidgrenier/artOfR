a <- list(u=5,v=12)
b <- list(w=13)
#list(a,b)
c(a,b,recursive=T)
z <- c(5,12,13); names(z) <- c('u','v','w'); z
c(u=5,v=12,w=13)
z
#z[[1]]
c(length(z), z[[1]]$u)
