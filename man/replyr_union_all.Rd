% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_rows.R
\name{replyr_union_all}
\alias{replyr_union_all}
\title{Union two tables.}
\usage{
replyr_union_all(tabA, tabB, ..., useDplyrLocal = TRUE,
  useSparkRbind = TRUE,
  tempNameGenerator = mk_tmp_name_source("replyr_union_all"))
}
\arguments{
\item{tabA}{not-NULL table with at least 1 row.}

\item{tabB}{not-NULL table with at least 1 row on same data source as tabA and common columns.}

\item{...}{force later arguments to be bound by name.}

\item{useDplyrLocal}{logical if TRUE use dplyr::bind_rows for local data.}

\item{useSparkRbind}{logical if TRUE try to use rbind on Sparklyr data}

\item{tempNameGenerator}{temp name generator produced by wrapr::mk_tmp_name_source, used to record dplyr::compute() effects.}
}
\value{
table with all rows of tabA and tabB (union_all).
}
\description{
Spark 2* union_all has issues ( https://github.com/WinVector/replyr/blob/master/issues/UnionIssue.md ),
and exposed union_all semantics differ from data-source back-end to back-end.
This is an attempt to provide a join-based replacement.
}
\examples{

d1 <- data.frame(x = c('a','b'), y = 1, stringsAsFactors= FALSE)
d2 <- data.frame(x = 'c', z = 1, stringsAsFactors= FALSE)
replyr_union_all(d1, d2, useDplyrLocal= FALSE)

}
