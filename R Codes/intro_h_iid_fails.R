## Example to show that h_iid fails and h_mh works
# target: N(0,1)
# proposal: N(x, 100)



## generate one sample
one_sample <- function(len = 1e5)
{
  x <- numeric(len) + 0.5
  prob <- numeric(len)
  for (i in 2:len)
  {
    y <- rnorm(1, x[i-1], 100)
    if(runif(1) < dnorm(y)/dnorm(x[i-1])*
       dnorm(x[i-1], 10, 100)/dnorm(y, 10, 100))
    {
      x[i] <- y
      prob[i]<-1
    }else{
      x[i] <- x[i-1]
    }
  }
  return(list(mc=x, acc.prob = mean(prob)))
}

set.seed(42)
x <- one_sample()

h_iid <- density(x$mc)$bw

pdf("./Figures/h_iid_fails_intro_plots.pdf", height = 5, width = 10)
par(mfrow=c(1,2))
ts.plot(x$mc[5e4:1e5], ylab="MC")
plot(density(x$mc), ylab = expression(hat(f[h])), xlab = "x",
     main = "", lty = 2, ylim = c(0, 0.6), col = "blue")
lines(density(rnorm(1e5)), col = "red")
legend("topright", legend = c("true density", "KDE(h_iid)"),
       lty = c(1,2,4), col = c("red", "blue"),
       cex = 0.8)
dev.off()