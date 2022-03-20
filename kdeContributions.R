## data
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)

## create a grid
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)

## window width
h <- 0.4

# create bumps with width h and shape "Gaussian"
bumps <- sapply(x, function(k) dnorm((xgrid - k)/h)/(n * h))

## Fig 7.2: Kernel estimate showing the contributions of Gaussian kernels evaluated
## for the individual observations with bandwidth h = 0.4.
pdf("kdectrb.pdf", height = 7, width = 7)
plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)),
     type = "l", xlab = "x", lwd = 1, col = "red", 
     main = "")
out <- apply(bumps, 2, function(b) lines(xgrid, b, col = "blue"))
dev.off()