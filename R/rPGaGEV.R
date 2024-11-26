#' The random generating function of the power Garima-generalized extreme value distribution(PGaGEV).
#'
#' This function generatings random numbers of PGaGEV distribution.
#'
#' The n random value of PGaGEV distribution based on the research paper in references.
#'
#' @param n number of observations.
#' @param mu location parameter.\code{mu}=[-Inf,Inf].
#' @param sigma scale parameter number 1. \code{sigma}>0.
#' @param xi shape parameter number 1. \code{xi}=[-Inf,Inf], where xi not equal to zero.
#' @param a scale parameter number 2. \code{a}>0.
#' @param b scale parameter number 3. \code{b}>0.
#' @param c shape parameter number 2. \code{c}=[-Inf,Inf].
#'
#' @references Kittipong Klinjan, Tipat Sottiwan and Sirinapa Aryuyuen (2024).
#' Extreme value analysis with new generalized extreme value distributions: a case study for risk analysis on pm2.5 and pm10 in pathum thani, thailand,
#' Commun. Math. Biol. Neurosci. 2024, 2024:100.DOI:10.28919/cmbn/8833.
#'
#' @importFrom LambertW W
#' @importFrom stats runif
#' @return the quantile values of PGaGEV distribution.
#' @export
#'
#' @examples
#' rPGaGEV(30,2,1,0.5,0.5,0.5,0.5)   #xi>0
#' rPGaGEV(30,2,1,-0.5,0.5,0.5,0.5)  #xi<0
rPGaGEV<-function(n,mu,sigma,xi,a,b,c){
  u<-runif(n,0,1)
  w1=LambertW::W(-(1-u)*(2+b)*exp(-2-b),branch=-1)
  w2=(-(1/b)*(w1+b+2))^(-1/c)
  w3=(1+w2)^(-1/a)
  w4=(-log(w3))^(-xi)
  qq=mu-(sigma/xi)*(1-w4)
  x=mu-(sigma/xi)*(1-w4)
  return(x)
}
