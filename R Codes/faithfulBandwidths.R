# R code showing how bandwidth influences the shape of the density estimate
# Data: Old Faithful geyser data

z <- density(faithful$waiting)
pdf("faithfulBWs.pdf", height = 7, width = 7)
hist(faithful$waiting, xlab = "Waiting times (in min.)", ylim = c(0, 0.05),
     probability = TRUE, main = "", col = "lightgray", border = "gray",
     density = 50, xlim = c(30,110))
lines(z,  main = "",
     ylab = expression(hat(f[h])), col = "red")
cat("The optimal bandwidth is:,",z$bw,"\n")
lines(density(faithful$waiting, bw=1), col = "blue", lty = 2)
lines(density(faithful$waiting, bw=10), col = "seagreen", lty = 3, lwd=2)
legend("topleft", legend=c("h(opt.) = 3.99","h = 1","h = 10"),
       lty = 1:3, lwd = c(1,1,2), col = c("red", "blue", "seagreen"),
       cex = 0.9, title = "Bandwidths")
dev.off()
