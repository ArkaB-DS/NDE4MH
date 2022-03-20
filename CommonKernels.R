## Density estimates of the geyser eruption data imposed on a histogram
## of the data.
x <- faithful$waiting
pdf("CommonKernels.pdf", height = 7, width = 7)
hist(x, xlab = "Waiting times (in min.)", ylab = "Relative Frequency",
     probability = TRUE, main = "", col = "lightgray", border = "gray",
     density = 50)
lines(density(x, width = 12), lty = 1, col = 1)
lines(density(x, width = 12, window = "rectangular"), lty = 2, col = 2)
lines(density(x, width = 12, window = "triangular"), lty = 3, col = 3)
lines(density(x, width = 12, window = "epanechnikov"), lty = 4, col = 4)
lines(density(x, width = 12, window = "biweight"), lty = 5, col = 5)
lines(density(x, width = 12, window = "cosine"), lty = 6, col = 6)
legend(title = "Kernels","topright", legend = c("Gaussian", "rectangular", "triangular", "Epanechnikov",
                             "biweight", "cosine"),
       lty = 1:6, col = 1:6, cex = 0.7)
dev.off()