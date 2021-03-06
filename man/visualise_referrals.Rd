% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/referral.R
\name{visualise_referrals}
\alias{visualise_referrals}
\title{Draw referral diagram}
\usage{
visualise_referrals(ps, bar.width = 10, padding = 10, height = 200,
  interval = 70)
}
\arguments{
\item{ps}{dataframe of pathway features}

\item{bar.width}{width of bars}

\item{padding}{padding of bars}

\item{height}{total height of bars}

\item{interval}{interval between stages}
}
\description{
Draw referral diagram
}
\examples{
ref <- visualise_referrals(pseudo.tb.p, bar.width=40)
print(ref)
}
