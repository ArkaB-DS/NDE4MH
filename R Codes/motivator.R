## Motivator for bump-killing
# target: Gamma(3, 1)
# proposal: N(x, 100)

# h <- 0.5
## generate one sample
fact <- 1/sqrt(2*pi)

expo <- function(x) exp(-(x^2)/2)*fact

n <- 1e5

one_sample <- function(len = 1e5)
{
  x <- rep(3, len)
  for (i in 2:len)
  {
    y <- rgamma(1, 3, 1.7)
    if(runif(1) < dgamma(y, 3, 1)*dgamma(x[i-1], 3, 1.7)/
       dgamma(x[i-1], 3, 1)/dgamma(y, 3, 1.7))
    {
      x[i] <- y
    }else{
      x[i] <- x[i-1]
    }
  }
  return(x)
}

x <- one_sample()
pdf("bk_trace_motivator.pdf", width = 7, height = 7)
ts.plot(x)
dev.off()
pdf("bk_density_plots_motivator.pdf", height = 7, width =7)
plot(density(x), ylab = expression(hat(f[h])), xlab = "",
     main = "", lty = 2, ylim = c(0, 0.45), lwd = 2)
lines(density(rgamma(1e5, 3, 1)), col = "red")
h_iid <- density(x)$bw
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
A <- A.hat(x)
h_mh <- A^(1/5)*h_iid
lines(density(x, bw = h_mh), col = "blue", lty = 3)

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

A_bk <- Tis(x)
h.bk <-  A_bk^(1/5)*h_iid
M <- 1e3

grid <- seq(from = 0, to = 12, length.out = M)
vals <- numeric(length = M)

for(i in 1:M)
{
  print(i)
  u <- grid[i]
  sum <- 0
  for(j in 1:n)
  {
    sum <- sum + expo((x[j] - u)/h.bk[j])/h.bk[j]
  }
  vals[i] <- sum/n
}

lines(grid, vals, type = "l", col = "green", lty = 4)

legend("topright", legend = c("true density", "KDE(h_iid)",
                              "KDE(h_mh)", "KDE(h_bk)"),
       lty = c(1,2,3,4), col = c("red", "black", "blue", "green"),
       cex = 0.8, lwd = c(1,2,1,2))


dev.off()

