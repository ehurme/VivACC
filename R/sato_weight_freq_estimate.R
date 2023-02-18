
# what is the relationship between wingbeat changes and weight?
m1 <- 1:1000
f1 <- 10
f2 <- 11

m2 <- (f2/f1)^2 * m1

plot(m2-m1)


m1 <- 100
f1 <- 10
f2 <- seq(10, 12, by = 0.01)
m2 <- (f2/f1)^2 * m1
plot(f2,m2)
