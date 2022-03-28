set.seed(42)
library(extremefit)
library(Deriv)
one_sample_1 <- function(len = 1e5)
{
  x <- numeric(len)
  for (i in 2:len)
  {
    y <- rnorm(1, x[i-1], 2.4)
    if(runif(1) < dnorm(y)/dnorm(x[i-1]))
    {
      x[i] <- y
    }else{
      x[i] <- x[i-1]
    }
  }
  return(x)
}

one_sample_2 <- function(len = 1e5)
{
  x <- numeric(len)
  for (i in 2:len)
  {
    y <- rnorm(1, x[i-1], 4)
    if(runif(1) < dnorm(y)/dnorm(x[i-1]))
    {
      x[i] <- y
    }else{
      x[i] <- x[i-1]
    }
  }
  return(x)
}

pi_3 <- function(x) ifelse(rbinom(1, 1, 0.5), dnorm(x+1), dnorm(x - 1.5))

one_sample_3 <- function(len = 1e5)
{
  x <- numeric(len)
  for (i in 2:len)
  {
    y <- rnorm(1, x[i-1], 4)
    if(runif(1) < pi_3(y)/pi_3(x[i-1]))
    {
      x[i] <- y
    }else{
      x[i] <- x[i-1]
    }
  }
  return(x)
}

one_sample_4 <- function(len = 1e5)
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

n <- 1e3
samp <- 1e3

mat1 <- matrix(0, nrow = n, ncol = samp)
# mat2 <- matrix(0, nrow = 1e5, ncol = 1e3)
# mat3 <- matrix(0, nrow = 1e5, ncol = 1e3)
# mat4 <- matrix(0, nrow = 1e5, ncol = 1e3)

for (i in 1:samp)
{
  mat1[,i] <- one_sample_1(n)
  print(i)
  # mat2[,i] <- one_sample_2()
  # mat3[,i] <- one_sample_3()
  # mat4[,i] <- one_sample_4()
}

# kernel <- function(x) {
#   # Probability that left <= x <= right
#   probability <- pnorm(2, mean = 0, sd = 1) - pnorm(-2, mean = 0, sd = 1)
#   return((x >= -2 & x <= 2) * dnorm(x, mean = 0, sd = 1) / probability)
# }

fact <- 1/((pnorm(2) - pnorm(-2))*sqrt(2*pi))

expo <- function(x) exp(-(x^2)/2)*fact

A.hat <- function(x)
{
  T <- numeric(length = n)
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

bins <- seq(from = -2, to = 2, length.out = 402)

mu_02 <- 0
mu_21 <- 0

for(i in 1:401)
{
  len <- bins[i + 1] - bins[i]
  mid <- (bins[i + 1] + bins[i])/2

  mu_02 <- mu_02 + len*(expo(mid)^2)
  mu_21 <- mu_21 + len*((mid^2) * expo(mid))
}

Deriv6 <- Deriv(Deriv(Deriv(Deriv(Deriv(Deriv(expo))))))

Deriv4 <- Deriv(Deriv(Deriv(Deriv(expo))))

I3val <- function(x, gk)
{
  val <- 0
  
  vec <- as.vector(sapply(x, function (y) y - x))
  vec <- vec/gk
  vec <- vec[vec > -2 && vec < 2]
  vec <- Deriv6(vec)
  val <- sum(vec)
  val <- -val/(n*n*(gk^7))
  return(val)
}

I2val <- function(x, gk)
{
  val <- 0
  
  vec <- as.vector(sapply(x, function (y) y - x))
  vec <- vec/gk
  vec <- vec[vec > -2 && vec < 2]
  vec <- Deriv4(vec)
  val <- sum(vec)
  val <- val/(n*n*(gk^5))
  return(val)
}

h_mh.hat <- function(x)
{
  sample.sd <- sd(x)
  
  I4 <- factorial(8)/((2*sample.sd)^9)*factorial(4)*(sqrt(pi))
  
  K6_0 <- Deriv6(0)
  K4_0 <- Deriv4(0)
  
  A <- A.hat(x)
  g3 <- abs((2*A*K6_0)/(mu_21*I4*n))^(1/9)
  I3 <- I3val(x, g3)
  
  g2 <- abs((2*A*K4_0)/(mu_21*I3*n))^(1/7)
  I2 <- I2val(x, g2)
  
  h_mh <- ((A*mu_02)/(mu_21*mu_21*I2*n))^(1/5)
  # val <- ((mu_21*mu_21*I2*(A^4)*(mu_02^4))^(1/5))*(5/(4*(n^(4/5))))
  
  return(h_mh)
}

mise <- function(x, h)
{
  bins <- seq(from = -2, to = 2, length.out = 201)
  sum <- 0
  for(i in 1:200)
  {
    len <- bins[i+1] - bins[i]
    mid <- (bins[i+1] + bins[i])/2
    ph.hat <- 0
    for(j in 1:n)
    {
      ph.hat <- ph.hat + expo((x[j] - mid)/h)
    }
    ph.hat <- ph.hat/(n*h)
    sum <- sum + len*(dnorm(mid) - ph.hat)^2
  }
  return(sum)
}

mise.h_mh.hat.avg <- 0

for(i in 1:samp)
{
  print(i)
  h <- h_mh.hat(mat1[,i])
  mise.h_mh.hat.avg <- mise.h_mh.hat.avg + mise(mat1[,i], h)
}
mise.h_mh.hat.avg <- mise.h_mh.hat.avg/samp
mise.h_mh.hat.avg*1e4


save(mat1, mat2, mat3, mat4, "simu_rep.RData")

pdf("samplePaths.pdf", height = 10, width = 10)
par(mfrow = c(4, 1))
plot.ts(mat4[1:1e4,1], ylim = c(0,10))
plot.ts(mat4[1:1e4,2], ylim = c(0,10))
plot.ts(mat4[1:1e4,3], ylim = c(0,10))
plot.ts(mat4[1:1e4,4], ylim = c(0,10))
dev.off()









