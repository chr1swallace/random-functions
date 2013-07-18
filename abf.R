## this is a function to calculation approximate BayesFactors from p values and MAF - for reference see Wakefield, J (2009) Bayes factors for genome-wide association studies: comparison with p-values.
## Genetic Epidemiology 33: 79–86.

abf <- function(p,maf, n0=9500, n1=6670, scale0=n0, scale1=n1) { # evidence for null - ie low ABF => support for alternative
z <- qnorm(p/2, lower.tail=FALSE)
x0 <- 0; x1 <- 1; x2 <- 2 # multiplicative model
d2 <- (1-maf)^2 * x0^2 + 2*maf*(1-maf)*x1 + maf^2 * x2^2
d1 <- (1-maf)^2 * x0 + 2*maf*(1-maf)*x1 + maf^2 * x2
V <- (n0 + n1) / ( n0 * n1 * (d2-d1) )
## scale
scale <- ((n0 + n1)/(scale0 + scale1)) * (scale0/n0) * (scale1/n1)
V <- V * scale
W <- ( log(1.5)/qnorm( 0.99, lower.tail=FALSE) )^2
VW <- V+W
2 * log(sqrt(VW/V) * exp( - z^2 * W / (2 * VW) ))
}
