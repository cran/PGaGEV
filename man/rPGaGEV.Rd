% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rPGaGEV.R
\name{rPGaGEV}
\alias{rPGaGEV}
\title{The random generating function of the power Garima-generalized extreme value distribution(PGaGEV).}
\usage{
rPGaGEV(n, mu, sigma, xi, a, b, c)
}
\arguments{
\item{n}{number of observations.}

\item{mu}{location parameter.\code{mu}=[-Inf,Inf].}

\item{sigma}{scale parameter number 1. \code{sigma}>0.}

\item{xi}{shape parameter number 1. \code{xi}=[-Inf,Inf], where xi not equal to zero.}

\item{a}{scale parameter number 2. \code{a}>0.}

\item{b}{scale parameter number 3. \code{b}>0.}

\item{c}{shape parameter number 2. \code{c}=[-Inf,Inf].}
}
\value{
the quantile values of PGaGEV distribution.
}
\description{
This function generatings random numbers of PGaGEV distribution.
}
\details{
The n random value of PGaGEV distribution based on the research paper in references.
}
\examples{
rPGaGEV(30,2,1,0.5,0.5,0.5,0.5)   #xi>0
rPGaGEV(30,2,1,-0.5,0.5,0.5,0.5)  #xi<0
}
\references{
Kittipong Klinjan, Tipat Sottiwan and Sirinapa Aryuyuen (2024).
Extreme value analysis with new generalized extreme value distributions: a case study for risk analysis on pm2.5 and pm10 in pathum thani, thailand,
Commun. Math. Biol. Neurosci. 2024, 2024:100.DOI:10.28919/cmbn/8833.
}
