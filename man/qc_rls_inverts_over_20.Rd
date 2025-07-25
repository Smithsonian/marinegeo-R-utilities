% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_rls_inverts_over_20.R
\name{qc_rls_inverts_over_20}
\alias{qc_rls_inverts_over_20}
\title{Check for invertebrate counts over 20}
\usage{
qc_rls_inverts_over_20(df)
}
\arguments{
\item{df}{}
}
\value{
a list of row numbers that are invert species with an invert count over 20
}
\description{
Check that the rls dataset is a dataframe and contains columns \code{Inverts} and \code{phylum}
finds rows where the phylum is not "Chordata" and the column \code{Inverts} is a numerical value over 20.
}
\examples{
rows_invert_counts_over_20 <- qc_rls_inverts_over_20(df)
}
