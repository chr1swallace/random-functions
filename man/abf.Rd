\name{abf}
\alias{abf}
\title{abf}
\usage{
  abf(p, maf, n0 = 9500, n1 = 6670, scale0 = n0,
    scale1 = n1)
}
\arguments{
  \item{p}{p value}

  \item{maf}{minor allele frequency}

  \item{n0}{number of controls}

  \item{n1}{number of cases}

  \item{scale0}{by default, =n0}

  \item{scale1}{by default, =n1}
}
\value{
  ABF
}
\description{
  Calculate approximate Bayes factors from p values and MAF
}
\details{
  this is a function to calculate approximate Bayes factors
  from p values and MAF - for reference see Wakefield, J
  (2009) Bayes factors for genome-wide association studies:
  comparison with p-values. Genetic Epidemiology 33: 79–86.
}
\author{
  Chris Wallace
}

