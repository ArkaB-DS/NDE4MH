library(ks)
set.seed(42)
samp <- 1e4
mus <- rbind(c(-5, 5), c(5, -5))
Sigmas <- rbind(matrix(c(2, 1, 1, 2), nrow=2), matrix(c(2, -1, -1, 2), nrow=2))
props <- c(0.5, 0.5)
original <- rmvnorm.mixt(n=samp, mus=mus, Sigmas=Sigmas, props=props)

one_sample <- function(len = 1e4)
{
  x <- matrix(0, ncol = 2, nrow = len)
  prob <- numeric(len)
  for (i in 2:len)
  {
    print(i)
    #y <-rnorm(1, x[i-1] - h/2*eval(deriv(~log(dnorm(x)), "x[i-1]")), sqrt(h)) 
    y <- rnorm(2, 0, 0.5)
    if(runif(1) < dmvnorm.mixt(y, mus, Sigmas, props)/
       dmvnorm.mixt(x[i-1, ], mus, Sigmas, props))
    {
      x[i, ] <- y
      prob[i] <- 1
    }else{
      x[i, ] <- x[i-1, ]
    }
  }
  return(list(mc=x, acc.prob = mean(prob)))
}

z <- one_sample()
z$acc.prob # 0.419
KDE<- kde(z$mc, H=Hpi.diag(z$mc))
plot(density(original[,1]), col = "red")
plot(density(z$mc[,1], bw = KDE$H[1,1]), col = "blue")

