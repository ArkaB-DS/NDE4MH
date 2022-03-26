## data
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5) + 2
n <- length(x)

## create a grid
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)

## window width
h <- 0.4
h2 <- 0.05
# create bumps with width h and shape "Gaussian"
 bumps <- sapply(x, function(k) dnorm((xgrid - k)/h)/(n * h))
bumps2 <- sapply(x, function(k) dgamma(xgrid, shape = k/(h2) + 1, scale = h2)/n)
## Fig 7.2: Kernel estimate showing the contributions of Gaussian kernels evaluated
## for the individual observations with bandwidth h = 0.4.
pdf("kdectrb_asym_sym.pdf", height = 7, width = 10)
par(mfrow=c(1,2))
plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)),
      type = "l", xlab = "x", lwd = 1, col = "red", 
      main = "")
out <- apply(bumps, 2, function(b) lines(xgrid, b, col = "blue"))
plot(xgrid, rowSums(bumps2), ylab = expression(hat(f)(x)),
     type = "l", xlab = "x", lwd = 1, col = "red", 
     main = "")
out2 <- apply(bumps2, 2, function(b) lines(xgrid, b, col = "seagreen"))
dev.off()