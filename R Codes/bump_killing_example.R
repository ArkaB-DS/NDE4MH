## Example for bump-killing
# target: N(0, 1)
# proposal: N(x, 100)

# h <- 0.5
## generate one sample
one_sample <- function(len = 1e5)
{
  x <- numeric(len) + 2
  prob <- numeric(len)
  for (i in 2:len)
  {
    #y <-rnorm(1, x[i-1] - h/2*eval(deriv(~log(dnorm(x)), "x[i-1]")), sqrt(h)) 
    y <- rnorm(1, 100, 100)
    if(runif(1) < dnorm(y)/dnorm(x[i-1]))
    {
      x[i] <- y
      prob[i]<-1
    }else{
      x[i] <- x[i-1]
    }
  }
  return(list(mc=x, acc.prob = mean(prob)))
}
x <- one_sample()
pdf("bk_trace.pdf", width = 7, height = 7)
ts.plot(x$mc)
dev.off()
pdf("bk_density_plots.pdf", height = 7, width =7)
plot(density(x$mc), ylab = expression(hat(f[h])), xlab = "",
     main = "", lty = 2, ylim = c(0, 0.45), lwd = 2)
lines(density(rnorm(1e5)), col = "red")
h_iid <- density(x$mc)$bw
A.hat <- function(x)
{
  n<-length(x)
  T <- numeric(n)
  for(i in 1:n)
  {
    j <- i
    while(x[i] == x[j] && j <= n)
    {
      T[i] <- T[i] + 1
      j <- j + 1
    }
  }
  
  return(sum(2*T - 1)/n)
}
A <- A.hat(x$mc)
h_mh <- A^(1/5)*h_iid
lines(density(x$mc, bw = h_mh), col = "blue", lty = 3)

Tis <- function(x)
{
  n<-length(x)
  T <- numeric(n)
  for(i in 1:n)
  {
    j <- i
    while(x[i] == x[j] && j <= n)
    {
      T[i] <- T[i] + 1
      j <- j + 1
    }
  }
  
  return(2*T-1)
}
A_bk <- Tis(x$mc)
h_bk <-  A_bk^(1/5)*h_iid
legend("topright", legend = c("true density", "KDE(h_iid)",
                              "KDE(h_mh)"),
       lty = c(1,2,3), col = c("red", "black", "blue"),
       cex = 0.8, lwd = c(1,2,1))
dev.off()
