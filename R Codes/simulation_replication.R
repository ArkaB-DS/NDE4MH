set.seed(42)
library(extremefit)
library(Deriv)
library(VGAM)
library(sn)

rm(list=ls())
len <- 1e4
prob <- numeric(len)
x <- numeric(len)
cnt <- 0
for (i in 2:len)
{
  y <- rsn(1, xi = x[i-1], omega = 4, alpha = 1)
  if(runif(1) < dsc(y, xi = 0, omega = 3, alpha = 3)/dsc(x[i-1], xi = 0, omega = 3, alpha = 3)*
     dsn(x[i-1], xi = y, omega = 4, alpha = 1)/
     dsn(y, xi = x[i-1], omega = 4, alpha = 1)
  )
  {
    x[i] <- y
    prob[i] <- 1
    cnt <- cnt + 1
  }else{
    x[i] <- x[i-1]
    cnt <- 0
  }
  if(cnt >= 100)
  {
    print("Rejections")
  }
}
#plot(density(x))
mean(prob)
ts.plot(x)

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

n <- 1e5
samp <- 1e3

mat1 <- matrix(0, nrow = n, ncol = samp)
mat2 <- matrix(0, nrow = 1e5, ncol = 1e3)
mat3 <- matrix(0, nrow = 1e5, ncol = 1e3)
mat4 <- matrix(0, nrow = 1e5, ncol = 1e3)

for (i in 1:samp)
{
  print(i)
  mat1[,i] <- one_sample_1(n)
  mat2[,i] <- one_sample_2()
  mat3[,i] <- one_sample_3()
  mat4[,i] <- one_sample_4()
}

save(mat1, mat2, mat3, mat4, file = "simu_rep.RData")

# fact <- 1/((pnorm(2) - pnorm(-2))*sqrt(2*pi))
fact <- 1/sqrt(2*pi)

expo <- function(x) exp(-(x^2)/2)*fact

mu02fn <- function(x) expo(x)^2

mu21fn <- function(x) x*x*expo(x)

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


mu_02 <- integrate(mu02fn, -2, 2)$value
mu_21 <- integrate(mu21fn, -2, 2)$value

Deriv6 <- Deriv(Deriv(Deriv(Deriv(Deriv(Deriv(expo))))))

Deriv4 <- Deriv(Deriv(Deriv(Deriv(expo))))

I23val <- function(x, gk, num)
{
  M <- 400
  g <- seq(min(x), max(x), length.out = M)
  # g <- g[2:M+1]
  c <- numeric(length = M)
  j <- 2
  xs <- sort(x)
  
  for(i in 1:n)
  {
    while(xs[i] > g[j] && j < M)
    {
      j <- j + 1
    }
    c[j-1] <- c[j-1] + (g[j] - xs[i])/(g[j] - g[j-1])
    c[j] <- c[j] + (xs[i] - g[j-1])/(g[j] - g[j-1])
  }

  if(num == 3)
  {
    # L <- min(floor((2*gk*(M-1))/(max(x) - min(x))), M - 1)
    # print(L)
    kappa <- numeric(length = M)
    
    for(i in 1:M)
    {
      temp <- ((max(x) - min(x))*i)/((M - 1)*gk)
      if(TRUE)
      {
        kappa[i] <- Deriv6(temp)/(n*gk)
      }
      else
      {
        kappa[i] <- 0
      }
    }
    kappa0 <- Deriv6(0)/(n*gk)
    # print(kappa)
    val <- 0
    for(j in 1:M)
    {
      inner <- 0
      for(l in (1-M):(M-1))
      {
        if((j-l) >= 1 && (j-l) <= M)
        {
          if(l == 0)
          {
            inner <- inner + c[j-l]*kappa0
          }
          else
          {
            inner <- inner + c[j-l]*kappa[abs(l)]
          }
        }
      }
      val <- val + inner*c[j]
    }
    # print(val)
    val <- val/(n*(gk^6))
    return(-val)
  }
  else{
    # L <- min(floor((2*gk*(M-1))/(max(x) - min(x))), M - 1)
    kappa <- numeric(length = M)
    
    for(i in 1:M)
    {
      temp <- ((max(x) - min(x))*i)/((M - 1)*gk)
      if(TRUE)
      {
        kappa[i] <- Deriv4(temp)/(n*gk)
      }
      else
      {
        kappa[i] <- 0
      }
    }
    kappa0 <- Deriv4(0)/(n*gk)
    # print(kappa)
    val <- 0
    for(j in 1:M)
    {
      inner <- 0
      for(l in (1-M):(M-1))
      {
        if((j-l) >= 1 && (j-l) <= M)
        {
          if(l == 0)
          {
            inner <- inner + c[j-l]*kappa0
          }
          else
          {
            inner <- inner + c[j-l]*kappa[abs(l)]
          }
        }
      }
      val <- val + inner*c[j]
    }
    # print(val)
    val <- val/(n*(gk^4))
    return(val)
  }
}

h_mh.hat <- function(x)
{
  sample.sd <- sd(x)
  
  I4 <- factorial(8)/((2*sample.sd)^9)*factorial(4)*(sqrt(pi))
  
  K6_0 <- Deriv6(0)
  K4_0 <- Deriv4(0)
  
  A <- A.hat(x)
  g3 <- abs((2*A*K6_0)/(mu_21*I4*n))^(1/9)
  I3 <- I23val(x, g3, 3)

  g2 <- abs((2*A*K4_0)/(mu_21*I3*n))^(1/7)
  I2 <- I23val(x, g2, 2)
  
  h_mh <- (abs((A*mu_02)/(mu_21*mu_21*I2*n)))^(1/5)
  
  # return((A^(1/5))*(density(x)$bw))
  return(h_mh)
}

h_bk <- function(x)
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
  A <- sum(2*T - 1)/n
  
  sample.sd <- sd(x)
  
  I4 <- factorial(8)/((2*sample.sd)^9)*factorial(4)*(sqrt(pi))
  
  K6_0 <- Deriv6(0)
  K4_0 <- Deriv4(0)

  g3 <- abs((2*A*K6_0)/(mu_21*I4*n))^(1/9)
  I3 <- I23val(x, g3, 3)
  
  g2 <- abs((2*A*K4_0)/(mu_21*I3*n))^(1/7)
  I2 <- I23val(x, g2, 2)
  
  h.bk <- numeric(length = n)
  for(i in 1:n)
  {
    h.bk[i] <- (abs(((2*T[i] - 1)*mu_02)/(mu_21*mu_21*I2*n)))^(1/5)
  }
  
  return(h.bk)
}

kde_bk <- function(x)
{
  h.bk <- h_bk(x)
  M <- 1e3
  
  grid <- seq(from = -3, to = 3, length.out = M)
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
  lines(grid, vals, type = "l", col = "green", lty = 1)
}

kde_mh <- function(x)
{
  h <- h_mh.hat(x)
  M <- 1e3
  
  grid <- seq(from = -3, to = 3, length.out = M)
  vals <- numeric(length = M)
  
  for(i in 1:M)
  {
    print(i)
    u <- grid[i]
    sum <- 0
    for(j in 1:n)
    {
      sum <- sum + expo((x[j] - u)/h)/h
    }
    vals[i] <- sum/n
  }
  lines(grid, vals, type = "l", col = "blue", lty = 3)
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

h_mh_all <- numeric(samp)
for(i in 1:samp)
{
  print(i)
  h_mh_all[i] <- h_mh.hat(mat4[,i])
  print(h_mh_all[i])
  # add <- mise(mat1[,i], h_mh_all[i])
  # print(add*1e4)
  # mise.h_mh.hat.avg <- mise.h_mh.hat.avg + add
}
# mise.h_mh.hat.avg <- mise.h_mh.hat.avg/samp
# mise.h_mh.hat.avg*1e4

for(i in 1:samp)
{
  print(i)
  
}


save(mat1, mat2, mat3, mat4, file = "simu_rep.RData")

pdf("samplePaths.pdf", height = 10, width = 10)
par(mfrow = c(4, 1))
plot.ts(mat4[1:1e4,1], ylim = c(0,10))
plot.ts(mat4[1:1e4,2], ylim = c(0,10))
plot.ts(mat4[1:1e4,3], ylim = c(0,10))
plot.ts(mat4[1:1e4,4], ylim = c(0,10))
dev.off()









