#' The cumulative distribution function (CDF) of the power Garima-generalized extreme value distribution(PGaGEV).
#'
#' This function calculated the CDF of PGaGEV distribution.
#'
#' The CDF of PGaGEV distribution based on the research paper in references.
#'
#' @param x vector of quantiles.There are three cases as follows:
#' 1) if xi>0, x=[(mu-sigma)/xi,Inf].2) if xi=0, x=[-Inf,Inf].3) if xi<0, x=[-Inf,(mu-sigma)/xi].
#' @param mu location parameter.\code{mu}=[-Inf,Inf].
#' @param sigma scale parameter number 1. \code{sigma}>0.
#' @param xi shape parameter number 1. \code{xi}=[-Inf,Inf].
#' @param a scale parameter number 2. \code{a}>0.
#' @param b scale parameter number 3. \code{b}>0.
#' @param c shape parameter number 2. \code{c}=[-Inf,Inf].
#'
#' @references Kittipong Klinjan, Tipat Sottiwan and Sirinapa Aryuyuen (2024).
#' Extreme value analysis with new generalized extreme value distributions: a case study for risk analysis on pm2.5 and pm10 in pathum thani, thailand,
#' Commun. Math. Biol. Neurosci. 2024, 2024:100.DOI:10.28919/cmbn/8833.
#'
#' @return the CDF of PGaGEV distribution.
#' @export
#'
#' @examples
#' pPGaGEV(1.2,2,1,0.5,0.5,0.5,.5) #xi=0.5
#' pPGaGEV(1.2,2,1,0.5,0.5,0.5,.5) #xi=0
#' pPGaGEV(1.2,2,1,0.5,0.5,0.5,.5) #xi=-0.5
#' x=c(1.2,1.3,1.4)
#' pPGaGEV(x,2,1,0.5,0.5,0.5,0.5)  #xi=0.5
pPGaGEV <- function(x,mu,sigma,xi,a,b,c)
{
  tauX <- (1+xi*((x-mu)/sigma))^(-1/xi)
  g  <-(1/sigma)*tauX^(xi+1)*exp(-tauX)
  G  <-exp(-tauX)
  Gc <-(G^a/(1-G^a))^c
  cdf<-1-(1+(b/(b+2))*Gc)*exp(-b*Gc)
  return(cdf)
}

