library(ks)
set.seed(42)
samp <- 1e5
mus <- rbind(c(-5,5), c(5,-5))
Sigmas <- rbind(matrix(c(1, -0.5, -0.5, 1), nrow=2), matrix(c(1, 0.5, 0.5, 1), nrow=2))
cwt <- 
props <- rep(1/2,2)
originals <- rmvnorm.mixt(n=samp, mus=mus, Sigmas=Sigmas, props=props)

̥## generate one sample
one_sample <- function(len = 1e5)
{
  x <- matrix(0, ncol = 2, nrow = len)
  prob <- numeric(len)
  for (i in 2:len)
  {
    y <- rnorm(2,x[i-1,],2.2)
    if(runif(1) < dmvnorm.mixt(y,mus,Sigmas, props)/dmvnorm.mixt(x[i-1,],mus,Sigmas, props)
    )
      
    {
      x[i,] <- y
      prob[i]<-1
    }else{
      x[i,] <- x[i-1,]
    }
  }
  return(list(mc=x, acc.prob = mean(prob)))
}
set.seed(42)
x <- one_sample()

KDE<-kde(x=x$mc, H=Hpi.diag(x$mc))
hs <- diag(sqrt(KDE$H))
plot(KDE)
plot(kde(x$mc, H = 2.5*Hpi.diag(x$mc)), add = TRUE, col = c("red", "green")) #replace 2.5 with A^(2/5)

