# sort example
z <- 10
x <- data.frame(rank=as.numeric(z),name=as.character("aaaa"),stringsAsFactors=FALSE)
x <- rbind(x,c(5,"llll"))
x <- rbind(x,c(15,"DDDD"))
x<-x[with(x,order(as.numeric(rank))),]
y
y[[1]][1]
y[[2]][1]