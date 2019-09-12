## consistent batch means and imse estimators of Monte Carlo standard errors
## author: Murali Haran

## Citation: Galin L. Jones, Murali Haran, Brian S. Caffo, and Ronald Neath, "Fixed-Width Output Analysis for Markov Chain Monte Carlo" (2006), Journal of the American Statistical Association, 101:1537--1547
## Geyer's initial monotone positive sequence estimator (Statistical Science, 1992)

imse <- function(outp,asymvar=FALSE)
  {
    chainAC <- acf(outp,type="covariance",plot = FALSE)$acf ## USE AUTOCOVARIANCES
    AClen <- length(chainAC)
    gammaAC <- chainAC[1:(AClen-1)]+chainAC[2:AClen]

    m <- 1
    currgamma <- gammaAC[1]
    k <- 1
    while ((k<length(gammaAC)) && (gammaAC[k+1]>0) && (gammaAC[k]>=gammaAC[k+1]))
      k <- k +1
 
    sigmasq <- -chainAC[1]+2*sum(gammaAC[1:k])

    if (asymvar) # return asymptotic variance
      return(sigmasq)
    
    mcse <- sqrt(sigmasq/length(outp))
    return(mcse)
  }