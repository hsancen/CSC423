#Sketch some graphs
x <- seq(0,10,0.01)
y = sqrt(x)
plot(x,y,type="l")

#Problem 10 on Review Questions

z <- (120 - 100)/15
pnorm(z)

z2 <- (180 - 100)/15
pnorm(z2)
ans <- 1 - pnorm(z2)

#examples of some graphs
x2 <- seq(-4, 4, 0.01)
y2 = dnorm(x2)
plot(x2,y2,type="l")

x2 <- seq(-4, 4, 0.01)
y2 = pnorm(x2)
plot(x2,y2,type="l")
