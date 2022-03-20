set.seed(42)
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

mat1 <- matrix(0, nrow = 1e5, ncol = 1e3)
mat2 <- matrix(0, nrow = 1e5, ncol = 1e3)
mat3 <- matrix(0, nrow = 1e5, ncol = 1e3)
mat4 <- matrix(0, nrow = 1e5, ncol = 1e3)

for (i in 1:1e3)
{
  mat1[,i] <- one_sample_1()
  mat2[,i] <- one_sample_2()
  mat3[,i] <- one_sample_3()
  mat4[,i] <- one_sample_4()
}

save(mat1, mat2, mat3, mat4, "simu_rep.RData")

pdf("samplePaths.pdf", height = 10, width = 10)
par(mfrow = c(4, 1))
plot.ts(mat4[1:1e4,1], ylim = c(0,10))
plot.ts(mat4[1:1e4,2], ylim = c(0,10))
plot.ts(mat4[1:1e4,3], ylim = c(0,10))
plot.ts(mat4[1:1e4,4], ylim = c(0,10))
dev.off()









