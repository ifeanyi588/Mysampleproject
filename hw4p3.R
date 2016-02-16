normalclt <- function(n, miu = 12, stdev = 3, q = 4.3, l = 4.3)
{
  tnorm <- vector()
  texp <- vector()
  tpoisson <- vector()
  for (i in 1:1000)
  {
    #generating the normal RV and then graphing the t statistics
    gen1 <- rnorm(n, miu, stdev)
    average <- mean(gen1)
    tstat1 = (average - miu)/(stdev/sqrt(n))
    tnorm <- c(tnorm, tstat1)
    
    #generating the exponential RV and then graphing its approximation to the normal using the t-statistics
    gen2 <- rexp(n, q)
    avrate <- n/sum(gen2)
    tstat2 = (avrate - (1/q))/((1/q)/sqrt(n))
    texp <- c(texp, tstat2)
    
    #generating a poisson RV and then graphing its approximation to the normal using the t-statistics
    gen3 <- rpois(n, l)
    avnumber <- sum(gen3)/n
    tstat3 <- (avnumber - l)/(l/sqrt(n))
    tpoisson <- c(tpoisson, tstat3)
  }
  par(mfrow = c(1,3))
  graphnorm <- hist(tnorm, main = paste("Normal Distribution, n = ", n, sep = ""), xlab = paste("T-statistic"), breaks = 50)
  graphexp <- hist(texp, main = paste("Exponential Distribution, n = ", n, sep = ""), xlab = paste("T-statistic"), breaks = 50)
  graphpoisson <- hist(tpoisson, main = paste("Poisson Distribution, n = ", n, sep = ""), xlab = paste("T-statistic"), breaks = 50)

}