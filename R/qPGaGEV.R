#' The quantile function of the power Garima-generalized extreme value distribution(PGaGEV).
#'
#' This function calculated the quantile values of PGaGEV distribution.
#'
#' The quantile function of PGaGEV distribution based on the research paper in references.
#'
#' @param p vector of probabilities.
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
#' @importFrom LambertW W
#' @return the quantile values of PGaGEV distribution.
#' @export
#'
#' @examples
#' qPGaGEV(0.1639605,2,1,0.5,0.5,0.5,0.5)
#' x=c(1.2,1.3,1.4)
#' p <- pPGaGEV(x,2,1,0.5,0.5,0.5,0.5)
#' qPGaGEV(p,2,1,0.5,0.5,0.5,0.5)

qPGaGEV<-function(p,mu,sigma,xi,a,b,c)
{
  w1=LambertW::W(-(1-p)*(2+b)*exp(-2-b),branch=-1)
  w2=(-(1/b)*(w1+b+2))^(-1/c)
  w3=(1+w2)^(-1/a)
  w4=(-log(w3))^(-xi)
  qq=mu-(sigma/xi)*(1-w4)
  return(qq)
}
