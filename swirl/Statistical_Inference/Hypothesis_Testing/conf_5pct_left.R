x <- seq(-8,8, length = 1000)
dat <- data.frame(x=x, y=dnorm(x))
g <- ggplot(dat, aes(x = x, y = y)) + geom_line(size = 1.5) + xlim(-4, 4) + scale_y_continuous(limits=c(0.0,max(dat$y)))
suppressWarnings(g <- g+ layer("area", stat="identity", position="identity",mapping = aes(x=ifelse(-9<x & x<qnorm(.05),x,NA)),
            params=list(fill="red",alpha=.5, na.rm=TRUE)) )
suppressWarnings(print(g))
