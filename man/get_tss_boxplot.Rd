% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydro.R
\name{get_tss_boxplot}
\alias{get_tss_boxplot}
\title{Get TSS Boxplot}
\usage{
get_tss_boxplot(data, stream, remove_outliers = FALSE, ...)
}
\arguments{
\item{data}{A data.frame with Stream, year, month and TSS cols.}

\item{stream}{A character vector of stream names}

\item{remove_outliers}{logical. Whether to remove outliers or not.}

\item{...}{Arguments to pass to appedix functions.}
}
\value{
A ggplot.
}
\description{
Get TSS Boxplot
}
