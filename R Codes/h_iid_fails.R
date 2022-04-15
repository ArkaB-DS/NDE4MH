## Example to show that h_iid fails and h_mh works
# target: N(0, 1)
# proposal: N(x, 100)

 
# fact <- 1/sqrt(2*pi)
# expo <- function(x) exp(-(x^2)/2)*fact

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

x <- one_sample()

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

# Tis <- function(x)
# {
#   n<-length(x)
#   T <- numeric(n)
#   for(i in 1:n)
#   {
#     j <- i
#     while(x[i] == x[j] && j <= n)
#     {
#       T[i] <- T[i] + 1
#       j <- j + 1
#     }
#   }
#   
#   return(2*T-1)
# }
# A_bk <- Tis(x$mc)
# h_bk <-  A_bk^(1/5)*h_iid
# 
# M <- 1e3
# grid <- seq(from = -3, to = 3, length.out = M)
# vals <- numeric(length = M)
# n<-length(x$mc)
# for(i in 1:M)
# {
#   print(i)
#   u <- grid[i]
#   sum <- 0
#   for(j in 1:length(x$mc))
#   {
#     sum <- sum + expo((x$mc[j] - u)/h_bk[j])/h_bk[j]
#   }
#   vals[i] <- sum/n
# }

pdf("h_iid_fails_plots.pdf", height = 5, width = 10)
par(mfrow=c(1,2))
ts.plot(x$mc[5e4:1e5], ylab="MC")
plot(density(x$mc), ylab = expression(hat(f[h])), xlab = "x",
     main = "", lty = 2, ylim = c(0, 0.6), col = "seagreen")
lines(density(rnorm(1e5)), col = "red")
lines(density(x$mc, bw = h_mh), col = "blue", lty = 4)
# lines(grid, vals, type = "l", col = "green", lty = 4)
legend("topright", legend = c("true density", "KDE(h_iid)",
                              "KDE(h_mh)"),
       lty = c(1,2,4), col = c("red", "seagreen", "blue"),
       cex = 0.8)
dev.off()