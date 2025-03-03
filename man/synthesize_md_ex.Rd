% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ex.R
\name{synthesize_md_ex}
\alias{synthesize_md_ex}
\title{Synthesize a fictional EX domain for single dose administration}
\usage{
synthesize_md_ex(
  dm,
  drug = "RS2023",
  route = "ORAL",
  form = "TABLET",
  dose = 500,
  epoch = "OPEN LABEL TREATMENT",
  treatment_duration = 50,
  missed_prob = 0.15,
  missed_doses = T,
  red_prob = 0.3
)
}
\arguments{
\item{dm}{The DM including the subject info for whom to synthesize EX.}

\item{drug}{The name of the drug to be administered.}

\item{dose}{The dose.}

\item{treatment_duration}{The treatment duration in days.}

\item{missed_prob}{Probability to miss doses.}

\item{missed_doses}{Switch whether to include randomly missed doses as boolean.}

\item{red_prob}{The dose reduction probability.}
}
\value{
The EX domain as data frame.
}
\description{
Synthesize a fictional EX domain for single dose administration
}
\keyword{internal}
